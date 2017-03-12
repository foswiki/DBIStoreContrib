# See bottom of file for license and copyright information.
package Foswiki::Contrib::DBIStoreContrib;

=begin TML

---+ package Foswiki::Contrib::DBIStoreContrib

Base functionality of the DBIStoreContrib. The methods here are common
to both the cache and the pure store usages of the contrib. They are
responsible for the construction and management of the database.

No other module in DBIStoreContrib need know anything about DBI (except
for the nature of a statement handle).

=cut

use strict;
use warnings;
use Assert;
use Foswiki       ();
use Foswiki::Func ();
use DBI           ();

our $VERSION = '1.3';          # plugin version is also locked to this
our $RELEASE = '9 Mar 2017';

# Global options, used to control tracing etc throughout the module
use constant TRACE_ALL => 1;
our %OPTS = (
    trace => {
        load    => TRACE_ALL,
        search  => TRACE_ALL,
        updates => TRACE_ALL,
        load    => TRACE_ALL
    },
    debug => 0
);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(%OPTS trace);

our $SHORTDESCRIPTION = 'Use DBI to implement a store using an SQL database.';

# Type identifiers.
# FIRST 3 MUST BE KEPT IN LOCKSTEP WITH Foswiki::Infix::Node
# Declared again here because the constants are not defined
# in Foswiki 1.1 and earlier
use constant {
    NAME   => 1,
    NUMBER => 2,
    STRING => 3,
};

# Additional types local to this module - must not overlap known
# types
use constant {
    UNKNOWN => 0,

    # Gap for 1.2 types HASH and META

    BOOLEAN  => 10,
    SELECTOR => 11,    # Temporary, synonymous with UNKNOWN

    VALUE => 12,
    TABLE => 13,

    # An integer type used during hoisting where DB doesn't support
    # a BOOLEAN type
    PSEUDO_BOOL => 14,
};

our $personality;    # personality module for the selected DSN
our $dbh;            # DBI handle

sub personality {
    unless ($personality) {

        # Custom code to put DB's into ANSI mode and clean up error reporting
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} =~ /^dbi:(.*?):/i;
        my $module = "Foswiki::Contrib::DBIStoreContrib::Personality::$1";

        eval "require $module";
        if ($@) {
            trace $@;
            die "Failed to load personality module $module";
        }
        $personality = $module->new();
    }
    return $personality;
}

# Used throughout the module
sub trace {
    if ( $OPTS{cli} ) {
        print STDERR join( "\n", @_ ) . "\n";
    }
    else {
        Foswiki::Func::writeDebug( join( "\n", @_ ) );
    }
}

# Connect on demand - PRIVATE
# If $session is defined, do a hard reset
sub _connect {
    my ($session) = @_;

    return 1 if $dbh && !$session;

    unless ($dbh) {

        if ($Foswiki::inUnitTestMode) {

            # Change the DSN to a SQLite test db, which is held in the data
            # area; that way it will be ripped down by -clean
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} =
"dbi:SQLite:dbname=$Foswiki::cfg{WorkingDir}/TemporarySQLiteCache";
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{Username} = '';
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{Password} = '';
        }

        trace "CONNECT $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN}..."
          if $OPTS{trace}{load};

        $dbh = DBI->connect(
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN},
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{Username},
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{Password},
            { RaiseError => 1, AutoCommit => 1 }
        ) or die $DBI::errstr;

        trace "Connected" if $OPTS{trace}{load};

        personality()->startup($dbh);
    }

    # Check if the DB is initialised with a quick sniff of the tables
    # to see if all the ones we expect are there
    if ( $personality->table_exists( 'metatypes', 'topic' ) ) {
        if ( $OPTS{trace}{load} ) {

            # Check metatypes integrity
            my $tables = $dbh->selectcol_arrayref('SELECT name FROM metatypes');
            foreach my $table (@$tables) {
                unless ( $personality->table_exists($table) ) {
                    trace "$table is in metatypes but does not exist";
                }
            }
        }
        return 1 unless ($session);
        trace "HARD RESET" if $OPTS{trace}{load};
    }
    elsif ( $OPTS{trace}{load} ) {
        trace "Base tables don't exist";
        ASSERT($session);
    }

    # Hard reset; strip down all existing tables

    # The metatypes table is how we know which tables are ours. Add
    # this to the schema.
    my %tables = map { $_ => 1 }
      keys %{ $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} };
    if ( $personality->table_exists('metatypes') ) {
        my $mts = $dbh->selectcol_arrayref('SELECT name FROM metatypes');
        foreach my $t (@$mts) {
            $tables{$t} = 1;
        }
    }

    foreach my $table ( keys %tables ) {
        if ( $personality->table_exists($table) ) {
            $dbh->do( 'DROP TABLE ' . $personality->safe_id($table) );
            trace "Dropped $table" if $OPTS{trace}{load};
        }
    }

    # No topic table, or we've had a hard reset
    trace "Loading DB schema" if $OPTS{trace}{load};
    _createTables();

    # We only preload after a hard reset
    if ( $session && !$Foswiki::inUnitTestMode ) {
        trace "Schema loaded; preloading content" if $OPTS{trace}{load};
        _preload($session);
        trace "DB preloaded" if $OPTS{trace}{load};
    }
    return 1;
}

# Create the table for the given META - PRIVATE
sub _createTable {
    my ( $tname, $tschema ) = @_;

    # Create table
    my @cols;
    my %constraint;    # indexed on constraint name
    while ( my ( $cname, $cschema ) = each %$tschema ) {

        # resolve pseudo-type
        $cschema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$cschema}
          unless ( ref($cschema) );
        my $s = $personality->safe_id($cname) . ' ' . $cschema->{type};
        if ( $cschema->{primary} ) {
            $s .= ' PRIMARY KEY';
        }
        if ( defined $cschema->{unique} ) {
            ASSERT( $cschema->{unique} =~ /^[A-Za-z]+$/i ) if DEBUG;
            $constraint{ $cschema->{unique} } ||= [];
            push( @{ $constraint{ $cschema->{unique} } }, $cname );
        }
        push( @cols, $s );
    }
    my $sn = $personality->safe_id($tname);
    my $ok = 0;
    while ( my ( $cons, $set ) = each %constraint ) {
        push( @cols,
                'CONSTRAINT '
              . $personality->safe_id($cons)
              . ' UNIQUE ('
              . join( ',', map { $personality->safe_id($_) } @$set )
              . ')' );
        $ok = 1;
    }
    my $cols = join( ',', @cols );
    my $sql = "CREATE TABLE $sn ( $cols )";
    trace $sql if $OPTS{trace}{load};
    $dbh->do($sql);

    # Add non-primary tables to the table of tables
    $dbh->do("INSERT INTO metatypes (name) VALUES ( '$tname' )")
      unless $tname eq 'topic' || $tname eq 'metatypes';

    # Create indexes
    while ( my ( $cname, $cschema ) = each %$tschema ) {

        # dereference _NAME
        $cschema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$cschema}
          unless ( ref($cschema) );
        next unless ( $cschema->{index} );
        my $sql =
            'CREATE INDEX '
          . $personality->safe_id("IX_${tname}_${cname}") . ' ON '
          . $personality->safe_id($tname) . '('
          . $personality->safe_id($cname) . ')';
        trace $sql if $OPTS{trace}{load};
        $dbh->do($sql);
    }
}

# Create all the base tables in the DB (including all
# default META: tables) - PRIVATE
sub _createTables {

    _createTable( 'metatypes',
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}->{metatypes} );
    while ( my ( $name, $schema ) =
        each %{ $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} } )
    {
        next if $name eq 'metatypes' || $name =~ /^_/;

        trace "Creating table for $name" if $OPTS{trace}{load};
        _createTable( $name, $schema );
    }
    $dbh->do('COMMIT') if $personality->{requires_COMMIT};
}

# Load all existing webs and topics into the DB (expensive)
sub _preload {
    my ($session) = @_;
    my $root      = Foswiki::Meta->new($session);
    my $wit       = $root->eachWeb();
    while ( $wit->hasNext() ) {
        my $web = $wit->next();
        _preloadWeb( $web, $session );
    }
    $dbh->do('COMMIT') if $personality->{requires_COMMIT};
}

# Preload a single web - PRIVATE
# pass web and topic name to insert() simply so it appears in stack traces
sub _preloadWeb {
    my ( $w, $session ) = @_;
    my $web = Foswiki::Meta->new( $session, $w );
    insert( $web, undef, "$web." );
    my $tit = $web->eachTopic();
    while ( $tit->hasNext() ) {
        my $t = $tit->next();
        my $topic = Foswiki::Meta->load( $session, $w, $t );
        trace "Preloading topic $w/$t" if $OPTS{trace}{load};
        insert( $topic, undef, "$web.$t" );
    }

    my $wit = $web->eachWeb();
    while ( $wit->hasNext() ) {
        _preloadWeb( $w . '/' . $wit->next(), $session );
    }
}

sub _convertToUTF8 {
    return $_[0] if ( $Foswiki::cfg{Site}{CharSet} eq 'utf-8' );
    my $text = shift;
    $text = Encode::decode( $Foswiki::cfg{Site}{CharSet}, $text );
    $text = Encode::encode( 'utf-8', $text );
    return $text;
}

sub _truncate {
    my ( $data, $size ) = @_;
    die "wanekr" . join( ' ', caller ) unless defined $data;
    die "wane" . join( ' ', caller ) unless defined $data;
    return $data unless defined($size) && length($data) > $size;
    Foswiki::Func::writeWarning( 'Truncating ' . length($data) . " to $size" )
      if $OPTS{trace}{load};
    return substr( $data, 0, $size - 3 ) . '...';
}

# Get the column schema for the given column.
sub _column {
    my ( $table, $column ) = @_;

    my $t = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$table};
    if ( ref($t) ) {
        my $c = $t->{$column};
        return $c if ref($c);

        # If the type name starts with an underscore, map to a top-level
        # type name
        if ( $c && $c =~ /^_/ ) {
            $c = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$c};
            return $c if ref($c);
        }
    }
    Foswiki::Func::writeWarning(
        "DBIStoreContrib: Could not determine a type for $table.$column")
      if DEBUG;
    return $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{_DEFAULT};
}

sub start {

    # Start a transaction
}

sub commit {

    # Commit a transaction
    $dbh->do('COMMIT') if $personality->{requires_COMMIT};
}

# PACKAGE PRIVATE support forced disconnection when a store shim is decoupled.
sub disconnect {
    if ($dbh) {
        $dbh->disconnect();    # SMELL: keep around in FCGI?
        undef $dbh;
    }
}

# Get the named table from the schema, or (if appropriate) create it
sub _findOrCreateTable {
    my ( $type, $mo ) = @_;

    my $schema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type};

    # Make sure it's registered, or we are auto-extending the schema
    return $schema if $schema;

    return undef
      unless ( $Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoloadUnknownMETA}
        && $type =~ /^[A-Z][A-Z0-9_]+$/ );

    # The table is not in the schema. Is it in the DB?
    if ( $personality->table_exists($type) ) {

        # Table is in the DB; pull the column names from there
        # and add them to the schema
        $schema = { map { $_ => '_DEFAULT' } $personality->get_columns($type) };
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type} = $schema;

        return $schema;
    }

    # The table is not in the DB either. Try deduce the schema
    # from the data.
    $schema = {
        tid => {
            type =>
              $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}->{TOPICINFO}
              ->{tid}->{type}
        }
    };

    # Check the entries to ensure we have picked up all the
    # columns. We read *all* entries so we get all columns.
    foreach my $item ( $mo->find($type) ) {
        foreach my $col ( keys(%$item) ) {
            $schema->{$col} ||= '_DEFAULT';
        }
    }
    trace "Creating fly table for $type" if $OPTS{trace}{load};
    _createTable( $type, $schema );

    $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type} = $schema;

    return $schema;
}

# Determine if the column can be used, adding it if that's allowed
sub _findOrCreateColumn {
    my ( $col, $schema, $type ) = @_;

    # The column might be in the schema but not in the DB
    # if there was a race condition and someone deleted the
    # table under us. Table deletion is very rare, and admin
    # only, so this is an acceptable risk.
    return $col if ( $schema->{$col} );

    return undef
      unless $Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoAddUnknownFields};

    # The column might be in the DB but not in
    # the schema. This may happen if previous
    # meta-data caused the column to be added.
    unless ( $personality->column_exists( $type, $col ) ) {
        my $sql =
            'ALTER TABLE '
          . $personality->safe_id($type) . ' ADD '
          . $personality->safe_id($col) . ' '
          . $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{_DEFAULT}
          ->{type};
        trace $sql if $OPTS{trace}{load};
        $dbh->do($sql);
    }

    trace "Added '$type.$col' to the schema" if $OPTS{trace}{load};

    # _column will give us the default type if
    # the column name isn't matched
    $schema->{$col} = _column( $type, $col );

    return $col;
}

=begin TML

---++ StaticMethod insert($meta [, $attachment])
Insert an object into the database

May throw an execption if SQL failed.

=cut

sub insert {
    my ( $mo, $attachment ) = @_;

    _connect();

    if ( defined $attachment ) {
        ASSERT( $mo->web )   if DEBUG;
        ASSERT( $mo->topic ) if DEBUG;

        # Note that we DO NOT explicitly add the META:FILEATTACHMENT
        # entry table here.
        # That is done at a much higher level in Foswiki::Meta when the
        # referring topic has it's meta-data rewritten. Here we (will)
        # simply load a serialised version of the attachment data, if
        # the =serialised= column is specified in the schema.
        if ( $personality->column_exists( 'FILEATTACHMENT', 'serialised' ) ) {

            # Pull in the attachment data
            my $tid =
              $dbh->selectrow_array( 'SELECT tid FROM topic '
                  . "WHERE web='"
                  . $mo->web
                  . " AND name='"
                  . $mo->topic
                  . "'" );
            ASSERT($tid) if DEBUG;

       # TODO:
       #           $dbh->do( 'UPDATE ' . $personality->safe_id('FILEATTACHMENT')
       #                     . " SET serialised='$data'"
       #                     . " WHERE tid='$tid' AND name='$attachment'" );
        }
    }
    elsif ( $mo->topic() ) {

        # SMELL: concurrency? what if two topics are inserted at the same time?
        my $tid = $dbh->selectrow_array('SELECT MAX(tid) FROM topic')
          || 0;
        $tid++;
        trace "\tInsert $tid" if $OPTS{trace}{load};
        my $text      = _convertToUTF8( $mo->text() );
        my $esf       = _convertToUTF8( $mo->getEmbeddedStoreForm() );
        my $topicName = _convertToUTF8( $mo->topic() );
        $dbh->do(
            'INSERT INTO topic (tid,web,name,text,raw) VALUES (?,?,?,?,?)',
            {}, $tid, $mo->web(), $topicName, $text, $esf );

        foreach my $type ( keys %$mo ) {

            # Make sure the table exists
            my $schema = _findOrCreateTable( $type, $mo );

            next unless $schema;

            # The table might be in the schema but not in the database
            # if it is deleted from the database while we are not looking.
            # Table deletion is very rare, and admin only, so this is an
            # acceptable risk.

            # Insert this row
            my $data = $mo->{$type};

            foreach my $item (@$data) {
                my @kns;

                # Check that the table has the columns to accept this data
                foreach my $kn ( keys(%$item) ) {
                    my $col = _findOrCreateColumn( $kn, $schema, $type );
                    push( @kns, $col ) if $col;
                }

                # All tables have 'tid'
                unshift( @kns, 'tid' );
                my $sql =
                    'INSERT INTO '
                  . $personality->safe_id($type) . ' ('
                  . join( ',', map { $personality->safe_id($_) } @kns )
                  . ") VALUES ("
                  . join( ',', map { '?' } @kns ) . ")";
                shift(@kns);

                trace "$sql [tid," . join(
                    ',',
                    map {
                        defined $item->{$_}
                          ? _truncate( $item->{$_}, 80 )
                          : 'NULL'
                    } @kns
                  )
                  . ']'
                  if $OPTS{trace}{load};

                $dbh->do(
                    $sql,
                    {},
                    $tid,
                    map {
                        defined $item->{$_}
                          ? _truncate(
                            _convertToUTF8( $item->{$_} ),
                            _column( $type, $_ )->{truncate_to}
                          )
                          : undef
                    } @kns
                );
            }
        }
    }
    else {
        # Currently no way to add a web. Webs are identified by
        # a unique search over the topics table.
    }
}

=begin TML

---++ StaticMethod remove($meta [, $attachment])

Delete a FW object from the database.

May throw an execption if SQL failed.

=cut

sub remove {
    my ( $mo, $attachment ) = @_;

    _connect();

    my $webName = _convertToUTF8( $mo->web() );

    if ( $mo->topic() ) {
        my $topicName = _convertToUTF8( $mo->topic() );

        my $sql = "SELECT tid FROM topic WHERE topic.web='" . $webName . "'";
        $sql .= " AND topic.name='" . $topicName . "'";
        my $tids = $dbh->selectcol_arrayref($sql);
        return unless scalar(@$tids);

        if ( defined $attachment ) {

            ASSERT( scalar(@$tids) == 1 ) if DEBUG;

            # Note that we DO NOT explicitly remove the META:FILEATTACHMENT
            # entry table here.
            # That is done at a much higher level in Foswiki::Meta when the
            # referring topic has it's meta-data rewritten.
            # Here we simply clear down the raw data stored for the attachment.
            if ( $personality->column_exists( 'FILEATTACHMENT', 'serialised' ) )
            {
                my $tid =
                  $dbh->selectrow_array( 'SELECT tid FROM topic '
                      . "WHERE web='"
                      . $webName
                      . "' AND name='"
                      . $topicName
                      . "'" );
                ASSERT($tid) if DEBUG;

      # TODO:
      #            $dbh->do( 'UPDATE ' . $personality->safe_id('FILEATTACHMENT')
      #                      . " SET serialised=''"
      #                      . " WHERE tid='$tid' AND name='$attachment'" );
            }
        }
        else {

            foreach my $tid (@$tids) {
                trace "\tRemove $tid" if $OPTS{trace}{load};
                my $tables =
                  $dbh->selectcol_arrayref('SELECT name FROM metatypes');
                foreach my $table ( 'topic', @$tables ) {
                    if ( $personality->table_exists($table) ) {
                        my $tn = $personality->safe_id($table);
                        $dbh->do("DELETE FROM $tn WHERE tid='$tid'");
                    }
                }
            }
        }
    }
}

=begin TML

---++ StaticMethod rename($metaOld, $metaNew)

Renames a FW object in the database.

May throw an execption if SQL failed.

=cut

sub rename {
    my ( $mo, $mn ) = @_;

    _connect();

    if ( $mo->web() && !$mo->topic() ) {
        my $oldWebName = _convertToUTF8( $mo->web() );
        my $newWebName = _convertToUTF8( $mn->web() );

        trace "\tRename web from $oldWebName to $newWebName"
          if $OPTS{trace}{load};
        $dbh->do(
            "UPDATE topic SET web = '$newWebName' WHERE web = '$oldWebName'");
    }
}

=begin TML

---++ StaticMethod query($sql) -> $sth

Perform an SQL query on the database, returning the statement handle.

May throw an exception if there is an error in the SQL.

=cut

sub query {
    my $sql = shift;

    _connect();
    my $sth = $dbh->prepare($sql);
    $sth->execute();
    return $sth;
}

=begin TML

---++ ObjectMethod reset($session)
Reset the DB by dropping existing tables (if they exist) and preloading.

=cut

sub reset {
    my ($session) = @_;

    # Connect with hard reset
    eval { _connect($session); };
    if ($@) {
        trace $@;
        die $@;
    }
}

1;
__DATA__

Author: Crawford Currie http://c-dot.co.uk

Module of Foswiki - The Free and Open Source Wiki, http://foswiki.org/, http://Foswiki.org/

Copyright (C) 2013-2014 Foswiki Contributors. All Rights Reserved.
Foswiki Contributors are listed in the AUTHORS file in the root
of this distribution. NOTE: Please extend that file, not this notice.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version. For
more details read LICENSE in the root of this distribution.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

As per the GPL, removal of this notice is prohibited.
