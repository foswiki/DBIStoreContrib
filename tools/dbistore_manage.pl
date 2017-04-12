#! /usr/bin/env perl
#
# Static TML -> HTML converter
#
# cd to the tools directory to run it

use strict;
use FindBin;
use lib "$FindBin::Bin/../bin";
use Carp;

BEGIN {
    do 'setlib.cfg';
}

use Getopt::Long ();
use Pod::Usage   ();

use Foswiki ();
use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace $TABLE_PREFIX);
use Foswiki::Meta ();

my @traces;
my $dbitrace;
my @loads;
my @reloads;
my $sql;
my $query;
my $reset;
my $web;
my $topic;

my $result = Getopt::Long::GetOptions(
    'dbitrace=s' => \$dbitrace,
    'help'       => sub {
        if ( $ARGV[0] eq 'trace' ) {
            print "trace options: " . join( ' ', sort keys %TRACE ) . "\n";
            exit 0;
        }
        else {
            Pod::Usage::pod2usage(
                -exitstatus => 0,
                -verbose    => 2,
            );
        }
    },
    'load=s'   => \@loads,
    'query=s'  => \$query,
    'reload=s' => \@reloads,
    'reset'    => \$reset,
    'sql:s'    => \$sql,
    'topic=s'  => \$topic,
    'trace=s'  => \@traces
);

foreach my $o ( split( /,/, join( ',', @traces ) ) ) {
    if ( $o eq 'all' ) {
        foreach my $k ( keys %TRACE ) {
            $TRACE{$k} = 1;
        }
    }
    else {
        $TRACE{$o} = 1;
    }
}
$TRACE{dbi} = $dbitrace;

binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";

#print "TRACE: " . join( ' ', grep { $TRACE{$_} } sort keys %TRACE ) . "\n";
$TRACE{cli} = 1;

my $fw      = new Foswiki();
my $opsDone = 0;               # Number of operations done

# Full reset
if ($reset) {
    Foswiki::Contrib::DBIStoreContrib::reset();
    $opsDone++;
}

sub _refresh {
    my ( $full, $topics ) = @_;
    for my $wt (@$topics) {
        if ( $wt eq '*' ) {
            $topics = ['*'];
            last;
        }
    }
    foreach my $wt (@$topics) {
        if ( $wt eq '*' ) {
            Foswiki::Contrib::DBIStoreContrib::load( Foswiki::Meta->new($fw),
                $full );
        }
        else {
            my ( $w, $t ) =
              $fw->normalizeWebTopicName( undef, Encode::decode_utf8($wt) );

            my $mo = Foswiki::Meta->load( $fw, $w, $t eq '*' ? undef : $t );
            Foswiki::Contrib::DBIStoreContrib::load( $mo, $full, $fw );
        }
    }
    $opsDone++;
}

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

# Like Data::Dumper, but more readable.
sub _dump {
    my ( $data, $indent ) = @_;

    $indent //= 0;
    my @e;
    my @br;

    if ( !ref($data) ) {
        return "undef" unless defined $data;
        return $data if $data =~ /^[0-9]+$/;
        return "'$data'";
    }
    elsif ( ref($data) eq 'ARRAY' ) {
        @br = qw/[ ]/;
        for ( my $i = 0 ; $i <= $#{$data} ; $i++ ) {
            push( @e, _dump( $data->[$i] ) );
        }
    }
    elsif ( ref($data) eq 'HASH' ) {
        @br = qw/{ }/;
        while ( my ( $k, $v ) = each %$data ) {
            push( @e, "$k =>" . _dump($v) );
        }
    }
    else {
        die "Can't _dump a " . ref($data);
    }
    my $s = $br[0] . join( ", ", @e ) . $br[1];
    return $s if ( _longest_line($s) < 80 );
    return "$br[0]\n" . join( ",\n", map { _indent($_) } @e ) . "\n$br[1]";
}

# Topic(s) to be reloaded
if ( scalar @reloads ) {
    _refresh( 1, \@reloads );
    $opsDone++;
}

# Topic(s) to be loaded
if ( scalar @loads ) {
    _refresh( 0, \@loads );
    $opsDone++;
}

if ($topic) {

    # Topic to base queries on
    ( $web, $topic ) = $fw->normalizeWebTopicName( undef, $topic );
}

# Foswiki query
if ( defined $query ) {
    if ( $query eq '-' ) {
        while (<>) {
            $query .= $_;
        }
    }
    $query = Encode::decode_utf8($query);
    require Foswiki::Contrib::DBIStoreContrib::HoistSQL;
    my $meta = Foswiki::Meta->new( $fw, $web, $topic );
    $query =
      $Foswiki::Plugins::SESSION->search->parseSearch( $query,
        { type => 'query' } );
    $sql = Foswiki::Contrib::DBIStoreContrib::HoistSQL::hoist($query);
    $sql = "SELECT web,name FROM \"${TABLE_PREFIX}topic\" WHERE $sql";
    $sql .= " AND name LIKE '$topic'" if $topic;
    $sql .= " AND web LIKE '$web'"    if $web;

    $opsDone++;
}

# SQL query
if ( defined $sql ) {
    if ( $sql eq '-' ) {
        while (<>) {
            $sql .= $_;
        }
    }
    my $rv = Foswiki::Contrib::DBIStoreContrib::query($sql);
    if ( $sql =~ /^\s*select\W/i ) {
        print _dump($rv) . "\n";
    }
    else {
        print "$sql OK\n";
    }
    $opsDone++;
}

unless ($opsDone) {
    Pod::Usage::pod2usage(
        -exitstatus => 0,
        -verbose    => 2,
    );
}

1;
__END__

=head1 tools/dbistore_manage.pl

Command-line management for DBIStoreContrib

=head1 SYNOPSIS

 perl -I bin tools/dbistore_manage.pl [options]

=head1 OPTIONS

=over 8

=item B<--reset>

Clear down the database, and recreate the base tables from the schema
described in C<LocalSite.cfg>. Does not load any webs or topics, unless
C<--load> or C<--reload> is also given.
Ensure the wiki is not in use before resetting the database!

=item B<--load> topic

Load the given topic if it is present in the backing store
but missing from the database.

C<--load *> can be used to load all missing topics.

=item B<--reload> topic

Reload the given topic from the backing store. If the topic is already in the DB
it is completely unloaded first before reloading. You can give as many C<--reload>
options as you want.

C<--reload *> can be used to reload all topics in the store.

=item B<--topic> topic

Set the topic for Foswiki queries (wildcards are *not* supported. If not
given, queries are executed over the entire database).

=item B<--query> TML query

Execute the given TML query (over the C<--topic> if given), and report the
result. Only one of C<--query> or C<--sql> may be given. Use B<--query -> to
read a query from standard input.

=item B<--sql> SQL

Execute the given SQL query over the DB, and report the result.
Only one of C<--query> or C<--sql> may be given. Use B<--sql -> to
read a query from standard input.

=item B<--trace>

Enable a trace option. C<--help trace> can be used to list trace keys.
C<--trace all> will switch on all trace options. Trace output is printed
to STDERR.

=item B<--dbitrace> trace

Set DBI trace options, which can be used to enable tracing of the DBI
module. "man dbi" shows the available trace flags, which can be
specified using an integer value or string.

=item B<--help>

Print this information. C<--help trace> will list available trace keys.

=back
