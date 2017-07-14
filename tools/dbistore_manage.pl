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
my $reload;
my $sql;
my $query;
my $reset;
my $web   = '*';
my $topic = '*';
my $clean;
my $check;
my $repair;

my $result = Getopt::Long::GetOptions(
    'check'      => \$check,
    'clean'      => \$clean,
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
    'load=s'  => \@loads,
    'query=s' => \$query,
    'reload'  => \$reload,
    'repair'  => \$repair,
    'reset'   => \$reset,
    'sql:s'   => \$sql,
    'topic=s' => \$topic,
    'trace=s' => \@traces
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

if ($clean) {
    Foswiki::Contrib::DBIStoreContrib::clean($fw);
    $opsDone++;
}

if ($check) {
    Foswiki::Contrib::DBIStoreContrib::checkDBIntegrity($repair);
    $opsDone++;
}

# Topic(s) to be loaded
if ( scalar @loads ) {
    foreach my $wt (@loads) {
        $wt .= '.*' unless $wt =~ /\./;
        my ( $w, $t ) = $fw->normalizeWebTopicName( undef, $wt );

        # Convert wildcards to perl regexes
        if ( $w eq '*' ) {
            $w = undef;
        }
        else {
            $w =~ s/\*/.*/g;
        }
        if ( $t eq '*' ) {
            $t = undef;
        }
        else {
            $t =~ s/\*/.*/g;
        }

        my $wo = Foswiki::Meta->new($fw);
        Foswiki::Contrib::DBIStoreContrib::load( $wo, $w, $t, $reload );
    }
    $opsDone++;
}

if ( $topic =~ /^(.*)\.(.*?)$/ ) {
    ( $web, $topic ) = ( $1, $2 );
}

$web   =~ s/\*/%/g;
$topic =~ s/\*/%/g;

# Foswiki query
if ( defined $query ) {
    if ( $query eq '-' ) {
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
    $sql = "SELECT #<web>,#<name>,#<tid> FROM #T<topic> WHERE $sql";
    $sql .= " AND #<name> LIKE '$topic'" if $topic && $topic ne '%';
    $sql .= " AND #<web> LIKE '$web'"    if $web   && $web   ne '%';

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
        foreach my $result (@$rv) {
            trace( join( ', ', @$result ) );
        }
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
described in C<LocalSite.cfg>. Does not load any webs or topics,
unless C<--load> is also given.  Ensure the wiki is not in use before
resetting the database!

=item B<--load> webtopic

Load the given web or topic if there is a more recent version in the backing
store or it is missing from the database. Simple * wildcards are supported.
For example --load Sandbox.ExampleTopic to load one topic, --load Webname.*
will load all topics in web Webname, and C<--load *.*> will load all topics in
all webs.

=item B<--reload>

Force all topics passed to --load to be loaded, even if the database
appears up to date.

C<--reload *> can be used to reload all topics in the store.

=item B<--topic> topic

Include a topic for --query. Supports * wildcards.
e.g. query a topic name over all webs using --query '*.TopicName'
or all topics in a web starting with "Fred" using --query 'Webname.Fred*'.
Only one --topic parameter can be given.

=item B<--query> TML query

Execute the given TML query (over the C<--topic> if given), and report the
result. Only one of C<--query> or C<--sql> may be given. Use B<--query -> to
read a query from standard input.

=item B<--sql> SQL

Execute the given SQL query over the DB, and report the result.
Only one of C<--query> or C<--sql> may be given. Use B<--sql -> to
read a query from standard input.

=item B<--clean>

In theory, nothing ever gets deleted from a Foswiki database. In
reality, sometimes you have to delete topics, and even entire webs. In
this case, the DBIStoreContrib database has to be cleaned, to delete
entries that no longer correspond to live topics. Cleaning is done
before any --query or --sql is executed.

=item B<--check> check database integrity

It's possible that a database operation might be incomplete, for
example if the database server crashes halfway through a deletion. In
this case it is possible for the database to become internally
inconsistent. If you see SQL error messages in the logs, you should
run with --check to determine if the database is internally
consistent. If it isn't, your can run --check again with the --repair
option, to repair any inconsistencies. Checking (and repair) are done
before and --query or --sql is executed.

=item B<--repair> repair database integrity

Does nothing on it's own, but when used with --check will repair any
inconsistencies found by --check.

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
