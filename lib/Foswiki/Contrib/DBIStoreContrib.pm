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
use File::Temp;

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
  trace personality insert remove rename expandIDs
  $TABLE_PREFIX %TRACE traceSQL
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
our $can_load = 1;   # Boolean, is the basic table integrity OK?

sub _ID {
    my $id = shift;

    # blunt instrument - protect against all non-word chars
    $id =~ s/([^\w])/'X'.ord($1)/ges;
    return "\"$id\"";
}

sub expandIDs {
    my $s = shift;
    $s =~ s/#T<(.*?)>/'"'.$TABLE_PREFIX.$1.'"'/ge;
    $s =~ s/#<(.*?)>/_ID($1)/ge;
    return $s;
}

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
        die "Failed to load personality module $personality: $@" if ($@);

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

# truncate and format a string
sub _fmt_truncate {
    my ( $data, $size ) = @_;
    $size //= 20;
    return $data unless length($data) > $size;
    my $len = '...' . length($data);
    return substr( $data, 0, $size - length($len) ) . $len;
}

# protected within this module
sub traceSQL {
    my $sql = shift;
    trace($sql);
    trace( _fmt( \@_ ) ) if scalar(@_);
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

sub error {
    trace(@_);
    unless ( $TRACE{cli} ) {

        # Repreint to STDERR
        $TRACE{cli} = 1;
        trace(@_);
        $TRACE{cli} = 0;
    }
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
    if ( personality->table_exists( 'metatypes', 'topic' ) ) {
        if ( $TRACE{action} ) {

            # Check metatypes integrity
            my $sth    = personality->sql('SELECT #<name> FROM #T<metatypes>');
            my $tables = $sth->fetchall_arrayref();
            foreach my $trow (@$tables) {
                my $table = personality->from_db( $trow->[0] );
                unless ( personality->table_exists($table) ) {
                    trace( $table, ' is in metatypes but does not exist' );
                }
            }
        }
    }
    else {
        $can_load = 0;
    }

    return $dbh;
}

# unwrap a stack of undos
sub _undo {
    my $undos = shift;
    foreach my $sql (@$undos) {
        eval { personality->sql(@$sql); };
        if ($@) {
            error("Undo failed");
            traceSQL(@$sql);
        }
    }
}

# Create the table for the given META - PRIVATE
sub _createTable {
    my ( $tname, $tschema, $undos ) = @_;

    # Create table
    my @cols;
    my %constraint;    # indexed on constraint name

    # Make a list of the SQL for the columns
    while ( my ( $cname, $cschema ) = each %$tschema ) {

        # resolve pseudo-type
        $cschema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$cschema}
          unless ( ref($cschema) );
        _basetype($cschema);
        my $csql = "#<$cname>" . ' ' . $cschema->{type};
        if ( $cschema->{primary} ) {
            $csql .= ' PRIMARY KEY';
        }

        my $default = $cschema->{default};
        if ( $cschema->{type} =~ /(CHAR|TEXT|BINARY)/i ) {
            $default = $dbh->quote( $cschema->{default} // '' );
        }
        elsif ( $cschema->{type} =~ /^BOOL/i ) {
            $default //= 'FALSE';
        }
        else {
            $default //= 0;
        }

        $csql .= " DEFAULT $default";
        if ( defined $cschema->{unique} ) {
            ASSERT( $cschema->{unique} =~ /^[A-Za-z]+$/i ) if DEBUG;
            $constraint{ $cschema->{unique} } ||= [];
            push( @{ $constraint{ $cschema->{unique} } }, $cname );
        }
        push( @cols, $csql );
    }

    # Add constraints (obviously not real columns)
    my $ok = 0;
    while ( my ( $cons, $set ) = each %constraint ) {
        push( @cols,
                "CONSTRAINT #<$cons> UNIQUE ("
              . join( ',', map { "#<$_>" } @$set )
              . ')' );
        $ok = 1;
    }
    push( @$undos, ["DROP TABLE #T<$tname>"] );

    personality->sql( "CREATE TABLE #T<$tname> ( " . join( ',', @cols ) . ")" );

    # Add non-primary tables to the table of tables
    unless ( $tname eq 'topic' || $tname eq 'metatypes' ) {
        unshift(
            @$undos,
            [
                'DELETE FROM #T<metatypes> WHERE #<name>=?',
                $TABLE_PREFIX . $tname
            ]
        );

        personality->sql( 'INSERT INTO #T<metatypes> (#<name>) VALUES ( ? )',
            $tname );
    }

    # Create indexes
    while ( my ( $cname, $cschema ) = each %$tschema ) {

        # dereference _NAME
        $cschema = $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{$cschema}
          unless ( ref($cschema) );
        next unless ( $cschema->{index} );
        my $id = $TABLE_PREFIX . $tname . '_' . ${cname} . '_INDEX';
        personality->sql("CREATE INDEX #<$id> ON #T<$tname> ( #<$cname> )");
    }
}

# Create all the base tables in the DB (including all
# default META: tables) - PRIVATE
sub _createTables {

    my @undos;

    eval {
        _createTable( 'metatypes',
            $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}->{metatypes},
            \@undos );
        while ( my ( $name, $schema ) =
            each %{ $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} } )
        {
            next if $name eq 'metatypes' || $name =~ /^_/;

            trace( 'Creating table for ', $name ) if $TRACE{action};
            _createTable( $name, $schema, \@undos );
        }
        $can_load = 1;
    };
    if ($@) {
        error("Table creation failed: $@");
        _undo( \@undos );
    }
}

# Determine if the web or topic represented by $meta is present in the DB.
# Returns an array of topid ids
sub _getTIDs {
    my $mo = shift;

    my $sql = "SELECT #<tid> FROM #T<topic> WHERE #<web>=?";

    my @params = ( site2uc( $mo->web() ) );
    if ( $mo->topic ) {
        $sql .= " AND #<name>=?";
        push( @params, site2uc( $mo->topic ) );
    }

    my $sth = personality->sql( $sql, @params );
    my $rv = $sth->fetchall_arrayref();
    return wantarray ? map { $_->[0] } @$rv : $rv->[0]->[0];
}

# Get the DB timestamp for a topic
sub _getTimestamp {
    my $mo = shift;

    my $sql = "SELECT #<timestamp> FROM #T<topic> WHERE #<web>=?";

    my @params = ( site2uc( $mo->web() ) );
    if ( $mo->topic ) {
        $sql .= " AND #<name>=?";
        push( @params, site2uc( $mo->topic ) );
    }

    my $sth = personality->sql( $sql, @params );
    my $rv = $sth->fetchall_arrayref();
    return undef unless $rv && $rv->[0] && $rv->[0]->[0];
    return $rv->[0]->[0];
}

# Recursively format a data structure for printing.
#    * =$data= - a hash, array or scalar value to format
#    * =\%options= - optional options.
#       * =maxline= sets the maximum line length, default 80
#       * =truncate= sets the maximum length of any single value, default 20
# Returns the formatted structure as a string.
# Mainly used for debugging, but also used by dbistore_manage.pl for
# formatting --query and --sql results.
sub _fmt {
    my ( $data, $options ) = @_;

    sub _longest_line {
        my $ml = 0;
        for ( split( "\n", shift ) ) {
            my $ll = length($_);
            $ml = $ll if $ll > $ml;
        }
        return $ml;
    }

    sub _indent {
        my ($s) = @_;
        return ' ' . join( "\n ", split( "\n", $s ) );
    }

    $options //= {};
    $options->{maxline} //= 80;

    my @e;
    my @br;

    if ( !ref($data) ) {
        return "undef" unless defined $data;
        return $data if $data =~ /^[0-9]+$/;
        return "'" . _fmt_truncate( $data, $options->{truncate} ) . "'";
    }
    elsif ( ref($data) eq 'ARRAY' ) {
        @br = qw/[ ]/;
        for ( my $i = 0 ; $i <= $#{$data} ; $i++ ) {
            push( @e, _fmt( $data->[$i], $options ) );
        }
    }
    elsif ( ref($data) eq 'HASH' ) {
        @br = qw/{ }/;
        while ( my ( $k, $v ) = each %$data ) {
            push( @e, "$k =>" . _fmt( $v, $options ) );
        }
    }
    elsif ( ref($data) ) {
        die "Can't format a " . ref($data);
    }
    my $s = $br[0] . join( ", ", @e ) . $br[1];
    return $s if ( _longest_line($s) < $options->{maxline} );
    return "$br[0]\n" . join( ",\n", map { _indent($_) } @e ) . "\n$br[1]";
}

=begin TML

---++ load( $webo, $wre, $tre, $reload )

Load (or reload) some part of the database from the backing store.
   * =$webo= - Foswiki::Meta object for a web to be recursively processed
   * =$wre= - regex that matches names of subwebs to process
    of undef for all subwebs
   * =$tre= - regex that matches names of topics to process
    of undef for all topics

If $reload is true, it will forcibly reload topics even if they are up
to date. Missing topics are always added.

=cut

sub load {
    my ( $wo, $wre, $tre, $reload ) = @_;

    getDBH();

    unless ($can_load) {
        die "Base metatypes and topic tables missing; do you need to --reset?";
    }

    trace( "Web ", $wo->getPath ) if $TRACE{action};
    my $wit = $wo->eachWeb();
    while ( $wit->hasNext() ) {

        my $w = ( $wo->web ? $wo->web . '/' : '' ) . $wit->next();
        next unless !defined($wre) || $w =~ /^$wre$/;

        # Load subweb
        my $swo = Foswiki::Meta->load( $wo->session, $w );
        load( $swo, $reload );
    }

    # No topics at root level
    return unless ( $wo->web() );

    my $tit = $wo->eachTopic();
    while ( $tit->hasNext() ) {
        my $t = $tit->next();
        next unless !defined($tre) || $t =~ /^$tre$/;

        # Load topic
        my $tmo = Foswiki::Meta->new( $wo->session, $wo->web, $t );
        my $stamp = _getTimestamp($tmo);
        if ( defined $stamp ) {
            unless ($reload) {
                my $info = $tmo->getRevisionInfo();
                if ( $stamp >= $info->{date} ) {

                    #trace($tmo->getPath . " is up to date") if $TRACE{action};
                    next;
                }
            }
            remove($tmo);
        }
        $tmo->load();
        insert($tmo);
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
# $type is the unprefixed table name
# $mo is the (optional) meta-object to be scanned for columns
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
# $col is the column name
# $tableSchema is the schema for the table
# $type is the unprefixed table name
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
        my @params;
        my $sql = "ALTER TABLE #T<$type> ADD #<$col> $tsch->{type} DEFAULT "
          . $dbh->quote( $tsch->{default} // '' );

        personality->sql( $sql, @params );
    }

    trace( 'Added ', $type, '.', $col . ' to the schema' ) if $TRACE{action};

    return $col;
}

sub _insertAttachment {
    my ( $mo, $attachment ) = @_;

    # Note that we DO NOT explicitly add the META:FILEATTACHMENT entry
    # table here. That is done at a much higher level in Foswiki::Meta
    # when the referring topic has it's meta-data rewritten.
    #
    # Here we simply load a serialised version of the attachment data
    # if the 'text' column is present in the schema and the StringifierContrib
    # is available.
    if (
        $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{FILEATTACHMENT}{text}
      )
    {

        eval { require Foswiki::Contrib::Stringifier; };
        if ($@) {
            die "FILEATTACHMENT has 'text', but cannot load Stringifier: $@";
        }

        trace( 'Serialising ', $mo->getPath, ':', $attachment )
          if $TRACE{action};

        # Pull in the attachment data
        my $tid = _getTIDs($mo);
        eval {
            my $suffix = ( $attachment =~ /(\..*?)$/ ? $1 : '.txt' );
            my $fh = $mo->openAttachment( $attachment, '<' );
            my ( $tfh, $tfn ) = File::Temp::tempfile(
                DIR    => $Foswiki::cfg{TempfileDir},
                UNLINK => 1,
                SUFFIX => $suffix
            );
            local $/;

            # Modern system; use a 512K buffer
            my $buffer;
            while ( read( $fh, $buffer, 524288 ) ) {
                print $tfh $buffer;
            }
            close($fh);
            close($tfh);
            my $data = Foswiki::Contrib::Stringifier->stringFor($tfn);
            personality->sql(
                'UPDATE #T<FILEATTACHMENT> SET text=? WHERE tid=? AND name=?',
                $data, $tid, $attachment );
        };
        if ($@) {
            error( 'Serialisation of ',
                $mo->getPath, ':', $attachment, ' failed:', $@ );
        }
    }
}

sub _insertTopic {
    my ( $mo, $undos ) = @_;

    # SMELL: concurrency? what if two topics are inserted at the same time?
    my $sth  = personality->sql('SELECT MAX(#<tid>) FROM #T<topic>');
    my $mtid = $sth->fetchall_arrayref()->[0];
    $mtid = $mtid->[0] if $mtid;
    my $tid = $mtid || 0;
    $tid++;
    trace( 'Insert ', $mo->web, '.', $mo->topic, '@', $tid )
      if $TRACE{action};

    unshift( @$undos, [ "DELETE FROM #T<topic> WHERE tid=?", $tid ] );

    personality->sql(
        'INSERT INTO #T<topic> ('
          . '#<tid>,#<web>,#<name>,#<timestamp>,#<text>,#<raw>'
          . ')VALUES (?,?,?,?,?,?)',
        $tid,
        site2uc( $mo->web() ),
        site2uc( $mo->topic() ),
        time(),
        site2uc( $mo->text() ),
        site2uc( $mo->getEmbeddedStoreForm() )
    );

    foreach my $type ( keys %$mo ) {

        next if $type =~ /^_/;

        # Make sure the table exists.
        my $tableSchema = _findOrCreateTable( $type, $mo );

        next unless $tableSchema;

        # The table might be in the schema but not in the database
        # if it is deleted from the database while we are not looking.
        # Table deletion is very rare, and admin only, so this is an
        # acceptable risk.

        # Insert this row
        my $data = $mo->{$type};

        foreach my $item (@$data) {

            # All tables have 'tid'
            my @kns = ('tid');

            # Check that the table has the columns to accept this data (or
            # they can be added)
            foreach my $kn ( keys(%$item) ) {
                my $col = _findOrCreateColumn( $kn, $tableSchema, $type );
                push( @kns, $col ) if $col;
            }

            my $phs = join( ',', map { '?' } @kns );
            my $cns = join( ',', map { "#<$_>" } @kns );
            shift(@kns);    # lose tid
            personality->sql(
                "INSERT INTO #T<$type> ($cns) VALUES ($phs)",
                $tid,
                map {
                    defined $item->{$_}
                      ? _truncate( site2uc( $item->{$_} ),
                        _column( $type, $_ )->{truncate_to} )
                      : undef
                } @kns
            );

            if ( $type eq 'FILEATTACHMENT' ) {
                insert( $mo, $item->{name} );
            }
        }
    }
}

=begin TML

---++ StaticMethod insert($meta [, $attachment])
Insert an object (web or topic) into the database

May throw an exception if SQL failed.

=cut

sub insert {
    my ( $mo, $attachment ) = @_;

    getDBH();

    my @undos;

    eval {

        if ( $mo->web && $mo->topic ) {
            if ( defined $attachment ) {
                _insertAttachment( $mo, $attachment, \@undos );
            }
            else {
                _insertTopic( $mo, \@undos );
            }
        }
        else {
            ASSERT( !$mo->topic )  if DEBUG;
            ASSERT( !$attachment ) if DEBUG;

            # Currently no way to add a web. Webs are identified by
            # a unique search over the topics table.
        }
    };
    if ($@) {
        error("DBIStoreContrib::insert failed: $@");
        _undo( \@undos );
    }
}

sub _remove {
    my ( $mo, $attachment ) = @_;

    my @tids = _getTIDs($mo);
    return unless scalar @tids;

    if ( defined $attachment ) {
        ASSERT( $mo->web && $mo->topic ) if DEBUG;

        # Note that we DO NOT explicitly remove the META:FILEATTACHMENT
        # entry table here.
        # That is done at a much higher level in Foswiki::Meta when the
        # referring topic has it's meta-data rewritten.

        # Here we simply clear down the serialised context stored for the
        # attachment, if present.
        if ( $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}
            {FILEATTACHMENT}{text} )
        {
            personality->sql(
                'UPDATE #T<FILEATTACHMENT> SET text=#<> WHERE tid=? AND name=?',
                $tids[0], $attachment
            );
        }
    }
    else {
        # Remove topic *or web*

        # Get list of our tables from metatypes
        my $sth = personality->sql('SELECT #<name> FROM #T<metatypes>');
        my @tables =
          grep { personality->table_exists($_) }
          map  { $_->[0] } @{ $sth->fetchall_arrayref() };

        foreach my $tid (@tids) {

            trace( 'Remove ', $mo->getPath(), '@', $tid )
              if $TRACE{action};

            # Iterate over all our tables removing the tid
            foreach my $tr ( 'topic', @tables ) {
                eval {
                    personality->sql( "DELETE FROM #T<$tr> WHERE #<tid>=?",
                        $tid );
                };
                if ($@) {
                    error( "Failed to delete $tid from $tr: ", $@ );
                }
            }
        }
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

    # Not possible to undo any part of a remove, so don't bother trying

    eval { _remove( $mo, $attachment ); };

    if ($@) {
        error(  'DBIStoreContrib::remove '
              . $mo->getPath()
              . ( $attachment ? ":$attachment" : '' )
              . ' failed: '
              . $@ );
    }
}

=begin TML

---++ StaticMethod rename($metaOld, $metaNew)

Renames a FW object in the database.

=cut

sub rename {
    my ( $mo, $mn ) = @_;

    ASSERT( $mo->web ) if DEBUG;

    getDBH();

    my $ow = site2uc( $mo->web() );

    eval {
        if ( $mo->topic && $mn->topic && $mn->topic ne $mo->topic )
        {
            # rename topic
            my $ot = site2uc( $mn->topic );
            my $nt = site2uc( $mn->topic );
            personality->sql(
                "UPDATE #T<topic> SET #<name>=? WHERE #<web>=? AND #<name>=?",
                $nt, $ow, $ot );
        }

        if ( $mo->web && $mn->web && $mn->web ne $mo->web ) {

            # Rename web
            my $nw = site2uc( $mn->web() );
            personality->sql( "UPDATE #T<topic> SET #<web>=? WHERE #<web>=?",
                $nw, $ow );
        }
    };
    if ($@) {
        error(  'DBIStoreContrib::rename '
              . $mo->getPath() . ' to '
              . $mn->getPath
              . ' failed: '
              . $@ );
    }
}

=begin TML
---++ StaticMethod clean($session)

Clean out dead topics (topics in the DB that have no corresponding
wiki topic)

=cut

sub clean {
    my $session = shift;

    getDBH();

    # Scan the DB and locate dead topics
    my @dead_tids;
    my $sth = personality->sql('SELECT #<tid>,#<web>,#<name> FROM #T<topic>');
    my $topics = $sth->fetchall_arrayref();
    foreach my $trow (@$topics) {
        my ( $tid, $web, $topic ) = @$trow;
        unless ( $session->webExists($web)
            && $session->topicExists( $web, $topic ) )
        {
            trace("Cleaning out $web.$topic") if $TRACE{action};
            push( @dead_tids, $tid );
        }
    }

    return unless scalar(@dead_tids);

    # Purge the dead tids from all our tables
    $sth = personality->sql('SELECT #<name> FROM #T<metatypes>');
    my $tables = $sth->fetchall_arrayref();
    my $dying = join( ',', @dead_tids );
    foreach my $trow ( @$tables, ['topic'] ) {
        my $table = $personality->from_db( $trow->[0] );
        personality->sql("DELETE FROM #T<$table> WHERE #<tid> IN ($dying)");
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

    my $sth = personality->sql($sql);
    my $rv  = $sth->fetchall_arrayref();

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

    if ($dbh) {
        $dbh->disconnect();
        undef $dbh;
    }

    getDBH();

    trace('HARD RESET') if $TRACE{action};

    # Build a list of tables se need to drop. Use a hash to avoid
    # duplicates.
    my %tables = map { $_ => 1 } (
        grep { !/^_/ }
          keys %{ $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema} }
    );

    # The metatypes table records tables we previously created, some
    # of which might have been dropped from the schema, so we need to check.
    if ( personality->table_exists('metatypes') ) {
        my $sth = personality->sql('SELECT #<name> FROM #T<metatypes>');
        my @mts = map { $_->[0] } @{ $sth->fetchall_arrayref() };
        foreach my $t (@mts) {
            $t = personality->from_db($t);
            $tables{$t} = 1;
        }
    }

    foreach my $table ( keys %tables ) {
        if ( personality->table_exists($table) ) {
            personality->sql("DROP TABLE #T<$table>");
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
