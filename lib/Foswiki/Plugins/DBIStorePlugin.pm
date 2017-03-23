# Plugin that provides the handlers for detecting store operations so the
# DBIStoreContrib can update the cache DB

package Foswiki::Plugins::DBIStorePlugin;

use strict;
use warnings;

use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace insert remove rename);
use Foswiki::Store ();
use Foswiki::Func  ();

our $VERSION           = $Foswiki::Contrib::DBIStoreContrib::VERSION;
our $RELEASE           = $Foswiki::Contrib::DBIStoreContrib::RELEASE;
our $NO_PREFS_IN_TOPIC = 1;
our $SHORTDESCRIPTION =
'Use DBI to implement searching using an SQL database. Supports SQL queries over Form data.';

sub initPlugin {

    foreach my $k (
        split( /\s+/, $Foswiki::cfg{Extensions}{DBIStoreContrib}{Trace} || '' )
      )
    {
        $TRACE{$k} = 1;
    }

    # If the getField method is missing, then get it from the BruteForce
    # module that it was moved from.
    require Foswiki::Store::QueryAlgorithms::DBIStoreContrib;
    unless ( Foswiki::Store::QueryAlgorithms::DBIStoreContrib->can('getField') )
    {
        trace('DBIStorePlugin: monkey-patching getField') if $TRACE{plugin};

        require Foswiki::Store::QueryAlgorithms::BruteForce;
        *Foswiki::Store::QueryAlgorithms::DBIStoreContrib::getField =
          \&Foswiki::Store::QueryAlgorithms::BruteForce::getField;
    }

    trace('DBIStorePlugin: initialised') if $TRACE{plugin};
    return 1;
}

# Store operations that *should* call the relevant store functions
#    moveTopic
#    moveWeb
#    saveTopic (no $new)
#    repRev(no $new)
#    delRev (no $new)
# Should call remove($old):
#    remove
# Some may not be called in the plugin, due to the inherent shittiness of
# the handler architecture.

# Required for most save operations
sub afterSaveHandler {

    # $text, $topic, $web, $error, $meta
    my $meta = $_[4];
    trace( "DBIStorePlugin::afterSaveHandler " . $meta->getPath() )
      if $TRACE{plugin};
    remove($meta);
    insert($meta);
}

# Required for a web or topic move
sub afterRenameHandler {
    my ( $oldWeb, $oldTopic, $olda, $newWeb, $newTopic, $newa ) = @_;

    trace(  "DBIStorePlugin::afterRenameHandler $oldWeb."
          . ( $oldTopic || '' ) . ':'
          . ($olda)
          . " to $newWeb."
          . ( $newTopic || '' ) . ':'
          . ( $newa || '' ) )
      if $TRACE{plugin};

    my $oldo =
      new Foswiki::Meta( $Foswiki::Plugins::SESSION, $oldWeb, $oldTopic );
    my $newo =
      new Foswiki::Meta( $Foswiki::Plugins::SESSION, $newWeb, $newTopic );

    Foswiki::Contrib::DBIStoreContrib::start();

    if ($oldTopic) {
        remove($oldo);    #, $olda );
        insert($newo);    #, $newa );
    }
    else {
        #rename web
        rename( $oldo, $newo );
    }
}

# Required for an upload
sub afterUploadHandler {
    my ( $attrs, $meta ) = @_;
    trace(  "DBIStorePlugin::afterUploadHandler "
          . $meta->getPath() . ':'
          . $attrs->{attachment} )
      if $TRACE{plugin};
    remove( $meta, $attrs->{attachment} );

    # The topic is saved too, but without invoking the afterSaveHandler :-(
    remove($meta);
    insert($meta);
    insert( $meta, $attrs->{attachment} );
}

1;
