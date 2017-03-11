#! /usr/bin/env perl
#
# Static TML -> HTML converter
#
# cd to the tools directory to run it

use strict;

BEGIN {

    sub setlibOnPath {
        foreach my $dir (@INC) {
            return 1 if ( -e "$dir/setlib.cfg" );
        }
        return 0;
    }

    unless ( setlibOnPath() ) {
        if ( defined $ENV{FOSWIKI_HOME} ) {
            unshift @INC, "$ENV{FOSWIKI_HOME}/bin";
            unless ( setlibOnPath() ) {
                unshift @INC, '../bin';
                unless ( setlibOnPath() ) {
                    print STDERR
"Can't locate setlib.cfg. Trying setting \%FOSWIKI_HOME or passing -I to perl\n";
                    exit 1;
                }
            }
        }
    }
    do 'setlib.cfg';
}

use Getopt::Long ();
use Pod::Usage   ();

use Foswiki ();
use Foswiki::Contrib::DBIStoreContrib qw(%OPTS trace);
use Foswiki::Meta ();

$OPTS{cli} = 1;

my @traceopts;
my @update;
my @sql;
my $reset;

my $result = Getopt::Long::GetOptions(
    'trace=s'  => \@traceopts,
    'reset'    => \$reset,
    'update=s' => \@update,
    'sql=s'    => \@sql,
    'help'     => sub {
        Pod::Usage::pod2usage( -exitstatus => 0, -verbose => 2 );
        exit 0;
    }
);

foreach my $o ( split( /,/, join( ',', @traceopts ) ) ) {
    if ( $o eq 'all' ) {
        foreach my $k ( keys %{ $OPTS{trace} } ) {
            $OPTS{trace}{$k} = 1;
        }
    }
    else {
        $OPTS{trace}{$o} = 1;
    }
}

my $fw = new Foswiki();

if ($reset) {
    if ( scalar(@update) > 0 || scalar(@sql) > 0 || scalar(@ARGV) > 0 ) {
        Pod::Usage::pod2usage( -exitstatus => 0, -verbose => 2 );
        die "--reset must be only parameter";
    }
    Foswiki::Contrib::DBIStoreContrib::reset($fw);
    exit 0;
}

foreach my $wn (@update) {
    my ( $web, $topic ) = $fw->normalizeWebTopicName($wn);
    my $meta = Foswiki::Meta->new( $fw, $web, $topic );
    trace "Update $web.$topic";
    Foswiki::Contrib::DBIStoreContrib::start();
    Foswiki::Contrib::DBIStoreContrib::remove($meta);
    Foswiki::Contrib::DBIStoreContrib::insert($meta);
    Foswiki::Contrib::DBIStoreContrib::commit();
}

foreach my $q (@sql) {
    my $sth = Foswiki::Contrib::DBIStoreContrib::query($q);
    $sth->dump_results();
}

my $meta = Foswiki::Meta->new( $fw, 'Sandbox', 'DBIStoreTest' );
print $meta->expandMacros( $meta->text() );

1;
__END__

=head1 tools/dbistore_manage.pl

Command-line management for DBIStoreContrib

=head1 SYNOPSIS

 perl -I bin tools/dbistore_manage.pl [options]

=head1 OPTIONS

=over 8

=item B<--update> topic

Unload the given topic from the DB, and reload it from the backing store.
You can give as many --update options as you want.

=item B<--sql> SQL

Execute the given SQL query over the DB, and report the result.

=item B<--reset>

Clear down the database, and reload all topics from scratch. Slow and expensive.
If --reset is given, other options and parameters are disabled. Ensure
the wiki is not in use before resetting the database!

=item B<--trace>

Enable a trace option. Options are: load, search, updates and sql. --trace all
will switch on all trace options. Trace output is printed to STDERR.

=item B<--help>

Print this information.


