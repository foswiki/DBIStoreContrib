#! /usr/bin/env perl
#
# Static TML -> HTML converter
#
# cd to the tools directory to run it

use strict;
use FindBin;
use lib "$FindBin::Bin/../bin";

BEGIN {
    do 'setlib.cfg';
}

use Getopt::Long ();
use Pod::Usage   ();

use Foswiki ();
use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace);
use Foswiki::Meta ();

my @traces;
my @loads;
my @reloads;
my $sql;
my $query;
my $reset;
my $web;
my $topic;

my $result = Getopt::Long::GetOptions(
    'trace=s'  => \@traces,
    'reset'    => \$reset,
    'reload=s' => \@reloads,
    'load=s'   => \@loads,
    'sql:s'    => \$sql,
    'query:s'  => \$query,
    'topic=s'  => \$topic,
    'help'     => sub {
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
    }
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

#print "TRACE: " . join( ' ', grep { $TRACE{$_} } sort keys %TRACE ) . "\n";
$TRACE{cli} = 1;

my $fw      = new Foswiki();
my $opsDone = 0;               # Number of operations done

# Full reset
if ($reset) {
    Foswiki::Contrib::DBIStoreContrib::reset($fw);
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
            my ( $w, $t ) = $fw->normalizeWebTopicName( undef, $wt );
            my $mo = Foswiki::Meta->load( $fw, $w, $t );
            Foswiki::Contrib::DBIStoreContrib::load( $mo, $full, $fw );
        }
    }
    $opsDone++;
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
    unless ($query) {
        while (<>) {
            $query .= $_;
        }
    }
    require Foswiki::Contrib::DBIStoreContrib::HoistSQL;
    my $meta = Foswiki::Meta->new( $fw, $web, $topic );
    $query =
      $Foswiki::Plugins::SESSION->search->parseSearch( $query,
        { type => 'query' } );
    $sql = Foswiki::Contrib::DBIStoreContrib::HoistSQL::hoist($query);
    $sql = "SELECT web,name FROM topic WHERE $sql";
    $sql .= " AND name LIKE '$topic'" if $topic;
    $sql .= " AND web LIKE '$web'"    if $web;

    $opsDone++;
}

# SQL query
if ( defined $sql ) {
    unless ($sql) {
        while (<>) {
            $sql .= $_;
        }
    }
    my $sth = Foswiki::Contrib::DBIStoreContrib::query($sql);
    $sth->dump_results();
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
result. Only one of C<--query> or C<--sql> may be given. If you don't specify
a query then the query will be read from standard input.

=item B<--sql> SQL

Execute the given SQL query over the DB, and report the result.
Only one of C<--query> or C<--sql> may be given. If you con't specify a SQL
string then the SQL will be read from standard input.

=item B<--trace>

Enable a trace option. C<--help trace> can be used to list trace keys.
C<--trace all> will switch on all trace options. Trace output is printed
to STDERR.

=item B<--help>

Print this information. C<--help trace> will list available trace keys.

=back
