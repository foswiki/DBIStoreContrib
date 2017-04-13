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
use Encode        ();

our $VERSION = '2.0';          # plugin version is also locked to this
our $RELEASE = '9 Apr 2017';

# Global options, used to control tracing etc throughout the module
our %TRACE = (
    action  => 0,
    hoist   => 0,
    plugin  => 0,
    search  => 0,
    sql     => 0,
    updates => 0
);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(
  trace personality insert remove rename
  $TABLE_PREFIX %TRACE
  NAME NUMBER STRING UNKNOWN BOOLEAN SELECTOR VALUE TABLE PSEUDO_BOOL);

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
# types declared in Foswiki::Infix::Node
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

our $TABLE_PREFIX;

our $personality;    # personality module for the selected DSN
our $dbh;            # DBI handle

# Get a reference to the personality module
sub personality {
    unless ($personality) {

        $TABLE_PREFIX =
          $Foswiki::cfg{Extensions}{DBIStoreContrib}{TablePrefix} // '';

        # Custom code to put DB's into ANSI mode and clean up error reporting
        $personality = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Personality};

        # If an explicit personality isn't given, try and guess it from the
        # DSN
        if (  !$personality
            && $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} =~
            /^dbi:(.*?):/i )
        {
            $personality = "Foswiki::Contrib::DBIStoreContrib::Personality::$1";
        }

        eval "require $personality";
        if ($@) {
            trace($@);
            die "Failed to load personality module $personality";
        }
        trace( 'Using ', $personality ) if $TRACE{action};
        $personality = $personality->new();
    }
    return $personality;
}

# _applyToStrings(data-to-convert, function-to-do-the-conversion)
# Because the DB only stores bytes, all data in the DB is stored UTF-8
# encoded. Need to convert to/from this encoding.
sub _applyToStrings {
    my ( $data, $fn ) = @_;

    if ( !ref($data) ) {
        return defined $data ? &$fn($data) : $data;
    }
    elsif ( ref($data) eq 'ARRAY' ) {
        for ( my $i = 0 ; $i <= $#{$data} ; $i++ ) {
            $data->[$i] = _applyToStrings( $data->[$i], $fn );
        }
        return $data;
    }
    elsif ( ref($data) eq 'HASH' ) {
        while ( my ( $k, $v ) = each %$data ) {
            my $nk = &$fn($k);    # assume keys are always scalars
            $v = _applyToStrings( $v, $fn );
            if ( $nk ne $k ) {
                delete $data->{$k};
                $data->{$nk} = $v;
            }
        }
        return $data;
    }
    else {
        die "Can't _applyToStrings a " . ref($data);
    }
}

# Convert from the site charset to unicode
sub site2uc {
    if ( !$Foswiki::UNICODE && $Foswiki::cfg{Site}{CharSet} ne 'utf-8' ) {

        # Decode site charset to unicode
        $_[0] = Encode::decode( $Foswiki::cfg{Site}{CharSet}, $_[0] );
    }
    return $_[0];
}

# Convert from unicode to the site charset
sub uc2site {

    if ( !$Foswiki::UNICODE && $Foswiki::cfg{Site}{CharSet} ne 'utf-8' ) {
        $_[0] = Encode::encode( $Foswiki::cfg{Site}{CharSet}, $_[0] );
    }
    return $_[0];
}

# Used throughout the module. Parameters are iterated through to generate
# the printed string. During the iteration, if the previous parameter
# ends in #, treat the current as a value. This is so
# we can write trace("X: #", $v) and get quotes generated iff $v
# is a string.
sub trace {

    # Determine if parameter is numeric or a string, print quotes if it's
    # a string.
    my @s;
    my $nv = 0;
    foreach (@_) {
        my $v = $_;
        if ($nv) {
            $v = (
                length(
                    do { no warnings "numeric"; $v & "" }
                )
            ) ? $v : "'$v'";
            $nv = 0;
        }
        else {
            $nv = ( $v =~ s/#$// );
        }
        push( @s, $v );
    }

    my $s = join( '', @s );
    if ( $TRACE{cli} ) {
        print STDERR "$s\n";
    }
    else {
        Foswiki::Func::writeDebug($s);
    }
}

=begin TML

---++ sub getDBH() -> $dbh

Connect on demand

=cut

sub getDBH {
    return $dbh if $dbh;

    # Pull in config trace options
    if ( my $tr = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Trace} ) {
        foreach my $o ( split( /[, |]/, $tr ) ) {
            if ( $o eq 'all' ) {
                foreach my $k ( keys %TRACE ) {
                    $TRACE{$k} = 1;
                }
            }
            else {
                $TRACE{$o} = 1;
            }
        }
    }

    if ($Foswiki::inUnitTestMode) {

        # Change the DSN to a SQLite test db, which is held in the data
        # area; that way it will be ripped down by -clean
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} =
          "dbi:SQLite:dbname=$Foswiki::cfg{WorkingDir}/TemporarySQLiteCache";
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Username} = '';
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Password} = '';
    }

    trace( 'CONNECT ', $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN} )
      if $TRACE{action};

    $dbh = DBI->connect(
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{DSN},
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Username},
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Password},
        { RaiseError => 1, AutoCommit => 1 }
    ) or die $DBI::errstr;

    # Automatically force UTF8 results when using ODBC
    # ODBC with odbc_has_unicode=true is untested.
    $dbh->{odbc_utf8_on} = 1 if defined $dbh->{odbc_has_unicode};

    # Acknowledge any overrides
    my $setup = $Foswiki::cfg{Extensions}{DBIStoreContrib}{DBIAttributes} // {};
    while ( my ( $k, $v ) = each %$setup ) {
        $dbh->{$k} = $v;
    }

    personality->startup($dbh);

    # Check if the DB is initialised with a quick sniff of the tables
    # to see if all the ones we expect are there
    if (
        personality->table_exists(
            $TABLE_PREFIX . 'metatypes',
            $TABLE_PREFIX . 'topic'
        )
      )
    {
        if ( $TRACE{action} ) {

            # Check metatypes integrity
            my $sql = "SELECT name FROM ${TABLE_PREFIX}metatypes";
            my $tables = personality->sql( 'selectcol_arrayref', $sql );
            foreach my $table (@$tables) {
                $table = personality->from_db($table);
                unless ( personality->table_exists($table) ) {
                    trace( $table, ' is in metatypes but does not exist' );
                }
            }
        }
    }
    elsif ( $TRACE{action} ) {
        trace('Base metatypes and topic tables don\'t exist');
    }

    return $dbh;
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
        _basetype($cschema);
        my $s = personality->identifier($cname) . ' ' . $cschema->{type};
        if ( $cschema->{primary} ) {
            $s .= ' PRIMARY KEY';
        }
        if ( defined $cschema->{default} ) {
            $s .=
              ' DEFAULT ' . personality->quoted_string( $cschema->{default} );
        }
        if ( defined $cschema->{unique} ) {
            ASSERT( $cschema->{unique} =~ /^[A-Za-z]+$/i ) if DEBUG;
            $constraint{ $cschema->{unique} } ||= [];
            push( @{ $constraint{ $cschema->{unique} } }, $cname );
        }
        push( @cols, $s );
    }
    my $sn = personality->identifier( $TABLE_PREFIX . $tname );
    my $ok = 0;
    while ( my ( $cons, $set ) = each %constraint ) {
        push( @cols,
                'CONSTRAINT '
              . personality->identifier($cons)
              . ' UNIQUE ('
              . join( ',', map { personality->identifier($_) } @$set )
              . ')' );
        $ok = 1;
    }
    my $cols = join( ',', @cols );
    my $sql = "CREATE TABLE $sn ( $cols )";
    personality->sql( 'do', $sql );

    # Add non-primary tables to the table of tables
    unless ( $tname eq 'topic' || $tname eq 'metatypes' ) {
        my $sql = "INSERT INTO ${TABLE_PREFIX}metatypes (name) VALUES ( "
          . personality->quoted_string("$TABLE_PREFIX$tname") . " )";
        personality->sql( 'do', $sql );
    }

    # Create indexes
    while ( my ( $cname, $cschema ) = each %$tschema ) {

        # dereference _NAME
        $cschema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$cschema}
          unless ( ref($cschema) );
        next unless ( $cschema->{index} );
        my $sql =
            'CREATE INDEX '
          . personality->identifier("IX_${tname}_${cname}") . ' ON '
          . personality->identifier( $TABLE_PREFIX . $tname ) . '('
          . personality->identifier($cname) . ')';
        personality->sql( 'do', $sql );
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

        trace( 'Creating table for ', $name ) if $TRACE{action};
        _createTable( $name, $schema );
    }
}

# Determine if the web or topic represented by $meta is present in the DB.
# Returns the topic tids if present, or false otherwise.
sub _getTIDs {
    my $mo = shift;

    my $sql =
        "SELECT tid FROM \"${TABLE_PREFIX}topic\" WHERE "
      . ( $mo->topic ? ' name=\'' . site2uc( $mo->topic ) . '\' AND ' : '' )
      . ' web=\''
      . site2uc( $mo->web() ) . '\'';

    # No need to decode, just numbers
    return personality->sql( 'selectrow_arrayref', $sql );
}

=begin TML

---++ load( $meta, $reload )

Load (or reload) the database from the backing store. If $meta is undef, it
will reload the entire DB from the root. If it is a web, it will reload all
topics in that web. If it is a topic, it will reload that topic.

If $reload is true, it will unload the topic from the DB (if it's there) and
load it from backing. Missing topics will be added.

If reload is false, it will skip topics already in the DB.

=cut

sub load {
    my ( $meta, $reload ) = @_;

    getDBH();

    if ( $meta->topic() ) {
        my $tids = _getTIDs($meta);
        if ( $tids && scalar(@$tids) ) {
            return unless ($reload);
            remove($meta);
        }
        insert($meta);
    }
    else {
        my $wit = $meta->eachWeb();
        while ( $wit->hasNext() ) {

            # Load subweb
            my $w = ( $meta->web ? $meta->web . '/' : '' ) . $wit->next();
            print STDERR "Web $w\n";
            my $wmo = Foswiki::Meta->load( $meta->session, $w );
            load( $wmo, $reload );
        }

        my $tit = $meta->eachTopic();
        while ( $tit->hasNext() ) {
            my $t = $tit->next();
            my $tmo = Foswiki::Meta->load( $meta->session, $meta->web, $t );
            load( $tmo, $reload );
        }
    }
}

sub _truncate {
    my ( $data, $size ) = @_;
    return $data unless defined($size) && length($data) > $size;
    Foswiki::Func::writeWarning( 'Truncating ' . length($data) . " to $size" );
    return substr( $data, 0, $size - 3 ) . '...';
}

# Expand the basetype in a schema column definition. This rewrites the
# schema so it doesn't have to be done more than once for each type.
sub _basetype {
    my $col = shift;

    return $col unless defined $col->{basetype};

    my $ctd =
      $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{ $col->{basetype} };

    unless ($ctd) {
        Foswiki::Func::writeWarning( 'No such basetype ' . $col->{basetype} );
        return $col;
    }

    $ctd = _basetype($ctd);    # recursive expansion

    while ( my ( $k, $v ) = each %$ctd ) {

        # Local definition overrides template
        $col->{$k} = $v unless defined $col->{$k};
    }
    delete $col->{basetype};
    return $col;
}

# Get the column schema for the given column.
sub _column {
    my ( $table, $column ) = @_;

    my $t = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$table};
    if ( ref($t) ) {
        my $c = $t->{$column};
        return _basetype($c) if ref($c);

        # If the type name starts with an underscore, map to a top-level
        # type name
        if ( $c && $c =~ /^_/ ) {
            $c = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$c};
        }
        return _basetype($c) if ref($c);
    }
    Foswiki::Func::writeWarning(
        "DBIStoreContrib: Could not determine a type for $table.$column")
      if DEBUG;
    return $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{_DEFAULT};
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

    my $tableSchema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type};

    # Make sure it's registered, or we are auto-extending the schema
    return $tableSchema if $tableSchema;

    return undef
      unless ( $Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoloadUnknownMETA}
        && $type =~ /^\w+$/ );

    # The table is not in the schema. Is it in the DB?
    if ( personality->table_exists($type) ) {

        # Table is in the DB; pull the column names and types from there
        # and add them to the schema so we don't go through this next time
        my $cols = personality->get_columns($type);
        my %tableSchema;
        while ( my ( $k, $v ) = each %$cols ) {
            $tableSchema{$k} = { type => $v };
        }
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type} =
          \%tableSchema;

        return $tableSchema;
    }

    # The table is not in the DB either. Try deduce the schema
    # from the data.
    $tableSchema = {
        tid => {
            type =>
              $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}->{topic}
              ->{tid}->{type}
        }
    };

    # Check the entries to ensure we have picked up all the
    # columns. We read *all* entries so we get all columns.
    foreach my $item ( $mo->find($type) ) {
        foreach my $col ( keys(%$item) ) {
            $tableSchema->{$col} ||= '_DEFAULT';
        }
    }
    trace( 'Creating fly table for ', $type ) if $TRACE{action};
    _createTable( $type, $tableSchema );

    $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$type} = $tableSchema;

    return $tableSchema;
}

# Determine if the column can be used, adding it if that's allowed
sub _findOrCreateColumn {
    my ( $col, $tableSchema, $type ) = @_;

    # The column might be in the schema but not in the DB
    # if there was a race condition and someone deleted the
    # table under us. Table deletion is very rare, and admin
    # only, so this is an acceptable risk.
    return $col if ( $tableSchema->{$col} );

    return undef
      unless $Foswiki::cfg{Extensions}{DBIStoreContrib}{AutoAddUnknownFields};

    # _column will give us the default type if
    # the column name isn't matched
    $tableSchema->{$col} = _column( $type, $col );
    my $tsch = $tableSchema->{$col};

    # The column might be in the DB but not in
    # the schema. If so, get the type from there.
    my $dbColType = personality->get_columns($type);
    $dbColType = $dbColType->{$col} if $dbColType;
    if ($dbColType) {
        $tsch->{type} = $dbColType;
    }
    else {
        # Must add the column to the DB
        $dbColType = $tsch->{type};
        my $sql =
            'ALTER TABLE '
          . personality->identifier( $TABLE_PREFIX . $type ) . ' ADD '
          . personality->identifier($col) . ' '
          . $dbColType
          . " DEFAULT "
          . (
            defined $tsch->{default}
            ? personality->quoted_string( $tsch->{default} )
            : "''"
          );
        personality->sql( 'do', $sql );
    }

    trace( 'Added ', $type, '.', $col . ' to the schema' ) if $TRACE{action};

    return $col;
}

=begin TML

---++ StaticMethod insert($meta [, $attachment])
Insert an object (web or topic) into the database

May throw an exception if SQL failed.

=cut

sub insert {
    my ( $mo, $attachment ) = @_;

    getDBH();

    if ( defined $attachment ) {
        ASSERT( $mo->web() )   if DEBUG;
        ASSERT( $mo->topic() ) if DEBUG;

        # Note that we DO NOT explicitly add the META:FILEATTACHMENT
        # entry table here.
        # That is done at a much higher level in Foswiki::Meta when the
        # referring topic has it's meta-data rewritten. Here we (will)
        # simply load a serialised version of the attachment data, if
        # the serialised column is present in the schema.
        if ( $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{FILEATTACHMENT}
            {serialised} )
        {

            # Pull in the attachment data
            my $tids = _getTIDs($mo);
            ASSERT( $tids && scalar @$tids ) if DEBUG;

            my $data = 'TODO: load and serialise attachments';
            my $sql =
                'UPDATE '
              . personality->identifier( $TABLE_PREFIX . 'FILEATTACHMENT' )
              . " SET serialised='$data'"
              . " WHERE tid='$tids->[0]' AND name='$attachment'";
            personality->sql( 'do', $sql );
        }
    }
    elsif ( $mo->topic() ) {

        # SMELL: concurrency? what if two topics are inserted at the same time?
        my $sql = "SELECT MAX(tid) FROM ${TABLE_PREFIX}topic";
        my $tid = personality->sql( 'selectrow_array', $sql ) || 1;
        $tid++;
        trace( 'Insert ', $mo->web, '.', $mo->topic, '@', $tid )
          if $TRACE{action};
        my $text      = site2uc( $mo->text() );
        my $esf       = site2uc( $mo->getEmbeddedStoreForm() );
        my $webName   = site2uc( $mo->web() );
        my $topicName = site2uc( $mo->topic() );
        $sql =
"INSERT INTO ${TABLE_PREFIX}topic (tid,web,name,text,raw) VALUES (?,?,?,?,?)";
        my @sqlp = ( $tid, $webName, $topicName, $text, $esf );
        personality->sql( 'do', $sql, {}, @sqlp );

        foreach my $type ( keys %$mo ) {

            next if $type =~ /^_/;

            # Make sure the table exists
            my $tableSchema = _findOrCreateTable( $type, $mo );

            next unless $tableSchema;

            # The table might be in the schema but not in the database
            # if it is deleted from the database while we are not looking.
            # Table deletion is very rare, and admin only, so this is an
            # acceptable risk.

            # Insert this row
            my $data = $mo->{$type};

            foreach my $item (@$data) {
                my @kns;

                # Check that the table has the columns to accept this data (or
                # they can be added)
                foreach my $kn ( keys(%$item) ) {
                    my $col = _findOrCreateColumn( $kn, $tableSchema, $type );
                    push( @kns, $col ) if $col;
                }

                # All tables have 'tid'
                unshift( @kns, 'tid' );
                my $sql =
                    'INSERT INTO '
                  . personality->identifier( $TABLE_PREFIX . $type ) . ' ('
                  . join( ',', map { personality->identifier($_) } @kns )
                  . ") VALUES ("
                  . join( ',', map { '?' } @kns ) . ")";
                shift(@kns);

                personality->sql(
                    'do', $sql,
                    {},
                    $tid,
                    map {
                        defined $item->{$_}
                          ? _truncate( site2uc( $item->{$_} ),
                            _column( $type, $_ )->{truncate_to} )
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

    getDBH();

    my $tids = _getTIDs($mo);
    return unless $tids && scalar @$tids;

    foreach my $tid (@$tids) {
        if ( defined $attachment ) {

            # SMELL: theoretically possible to remove an attachment on
            # all topics in a web?
            # Note that we DO NOT explicitly remove the META:FILEATTACHMENT
            # entry table here.
            # That is done at a much higher level in Foswiki::Meta when the
            # referring topic has it's meta-data rewritten.

            # Here we simply clear down the raw data stored for the
            # attachment, if present.
            if ( $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}
                {FILEATTACHMENT}{serialised} )
            {
                my $sql =
                    "SELECT tid FROM ${TABLE_PREFIX}topic WHERE web='"
                  . $mo->web
                  . "' AND name='"
                  . $mo->topic . "'";
                my $tid = personality->sql( 'selectrow_array', $sql );
                ASSERT($tid) if DEBUG;

                $sql =
                    'UPDATE '
                  . personality->identifier( $TABLE_PREFIX . 'FILEATTACHMENT' )
                  . " SET serialised=''"
                  . " WHERE tid='$tid' AND name='$attachment'";
                personality->sql( 'do', $sql );
            }
        }
        else {
            trace( 'Remove ', $mo->web, '.', $mo->topic, '@', $tid )
              if $TRACE{action};
            my $sql = "SELECT name FROM ${TABLE_PREFIX}metatypes";
            my $tables = personality->sql( 'selectcol_arrayref', $sql );
            foreach my $table ( 'topic', @$tables ) {
                my $t = $TABLE_PREFIX . $table;
                if ( personality->table_exists($t) ) {
                    my $tn = personality->identifier($t);
                    $sql = "DELETE FROM $tn WHERE tid='$tid'";
                    personality->sql( 'do', $sql );
                }
            }
        }
    }
}

=begin TML

---++ StaticMethod rename($metaOld, $metaNew)

Renames a FW object in the database.

May throw an exception if SQL failed.

=cut

sub rename {
    my ( $mo, $mn ) = @_;

    getDBH();

    if ( $mo->web() && !$mo->topic() ) {
        my $oldWebName = site2uc( $mo->web() );
        my $newWebName = site2uc( $mn->web() );

        my $sql =
"UPDATE ${TABLE_PREFIX}topic SET web = '$newWebName' WHERE web = '$oldWebName'";
        personality->sql( 'do', $sql );
    }
}

=begin TML

---++ StaticMethod query($sql) -> $rv

Perform an SQL query on the database, returning the return value.

May throw an exception if there is an error in the SQL.

The return value is decoded to the site charset.

=cut

sub query {
    my $sql = shift;

    getDBH();
    my $sth = personality->sql( 'prepare', $sql );
    $sth->execute();
    my $rv = $sth->fetchall_arrayref();
    $rv = _applyToStrings(
        $rv,
        sub {
            return uc2site( personality->from_db( $_[0] ) );
        }
    );
    return $rv;
}

=begin TML

---++ ObjectMethod reset()
Reset the DB by dropping existing tables (if they exist) and recreating
the tables specified by the configuration schema.

=cut

sub reset {

    # Connect with hard reset

    trace('HARD RESET') if $TRACE{action};

    if ($dbh) {
        $dbh->disconnect();
        undef $dbh;
    }

    getDBH();

    # The metatypes table is how we know which tables are ours. Add
    # this to the schema.
    my %tables = map { $TABLE_PREFIX . $_ => 1 } (
        grep { !/^_/ }
          keys %{ $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} }
    );
    if ( personality->table_exists("${TABLE_PREFIX}metatypes") ) {
        my $sql = "SELECT name FROM ${TABLE_PREFIX}metatypes";
        my $mts = personality->sql( 'selectcol_arrayref', $sql );
        foreach my $t (@$mts) {
            $t = personality->from_db($t);
            $tables{$t} = 1;
        }
    }

    foreach my $table ( keys %tables ) {
        if ( personality->table_exists($table) ) {
            my $sql = 'DROP TABLE ' . personality->identifier($table);
            personality->sql( 'do', $sql );
        }
    }

    trace('Loading DB schema') if $TRACE{action};
    _createTables();

    trace('Schema loaded') if $TRACE{action};
}

1;
__DATA__

Author: Crawford Currie http://c-dot.co.uk

Module of Foswiki - The Free and Open Source Wiki, http://foswiki.org/, http://Foswiki.org/

Copyright (C) 2013-2017 Foswiki Contributors. All Rights Reserved.
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
