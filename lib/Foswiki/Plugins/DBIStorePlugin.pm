# This is a plugin stub used to support the DBIStoreContrib with Foswiki
# versions < 1.2 that lack the Foswiki::Store::recordChange function.

package Foswiki::Plugins::DBIStorePlugin;

use strict;
use warnings;

use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace);
use Foswiki::Store ();
use Foswiki::Func  ();

our $VERSION           = $Foswiki::Contrib::DBIStoreContrib::VERSION;
our $RELEASE           = $Foswiki::Contrib::DBIStoreContrib::RELEASE;
our $NO_PREFS_IN_TOPIC = 1;
our $SHORTDESCRIPTION =
'Use DBI to implement searching using an SQL database. Supports SQL queries over Form data.';

sub initPlugin {

    foreach my $k (
        split( /\s+/, $Foswiki::cfg{Extensions}{DBIStoreContrib}{Trace} ) )
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

sub commonTagsHandler {

    # Normally preloading only occurs when the DB first connects, which
    # only happens when a topic is moved or saved. To short-circuit this,
    # the plugin supports the "?dbistore_reset" parameter, which will
    # do that chore. Only admins can call it. It's done this late in
    # the pipeline to ensure plugins have had a chance to register META
    # requirements.
    if ( Foswiki::Func::getRequestObject->param('dbistore_reset')
        && Foswiki::Func::isAnAdmin() )
    {
        trace('DBIStorePlugin: Resetting') if $TRACE{plugin};
        Foswiki::Func::getRequestObject->delete('dbistore_reset');
        Foswiki::Contrib::DBIStoreContrib::reset($Foswiki::Plugins::SESSION);
    }
    elsif ( Foswiki::Func::getRequestObject->param('dbistore_update') ) {
        my ( $text, $topic, $web, $included, $meta ) = @_;
        Foswiki::Func::getRequestObject->delete('dbistore_update');
        trace( 'DBIStorePlugin: Update ' . $meta->getPath() )
          if $TRACE{plugin};
        Foswiki::Contrib::DBIStoreContrib::start();
        Foswiki::Contrib::DBIStoreContrib::remove($meta);
        Foswiki::Contrib::DBIStoreContrib::insert($meta);
        Foswiki::Contrib::DBIStoreContrib::commit();
    }
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
    Foswiki::Contrib::DBIStoreContrib::start();
    Foswiki::Contrib::DBIStoreContrib::remove($meta);
    Foswiki::Contrib::DBIStoreContrib::insert($meta);
    Foswiki::Contrib::DBIStoreContrib::commit();
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
        Foswiki::Contrib::DBIStoreContrib::remove($oldo);    #, $olda );
        Foswiki::Contrib::DBIStoreContrib::insert($newo);    #, $newa );
    }
    else {
        #rename web
        Foswiki::Contrib::DBIStoreContrib::rename( $oldo, $newo );
    }

    Foswiki::Contrib::DBIStoreContrib::commit();
}

# Required for an upload
sub afterUploadHandler {
    my ( $attrs, $meta ) = @_;
    trace(  "DBIStorePlugin::afterUploadHandler "
          . $meta->getPath() . ':'
          . $attrs->{attachment} )
      if $TRACE{plugin};
    Foswiki::Contrib::DBIStoreContrib::start();
    Foswiki::Contrib::DBIStoreContrib::remove( $meta, $attrs->{attachment} );

    # The topic is saved too, but without invoking the afterSaveHandler :-(
    Foswiki::Contrib::DBIStoreContrib::remove($meta);
    Foswiki::Contrib::DBIStoreContrib::insert($meta);
    Foswiki::Contrib::DBIStoreContrib::insert( $meta, $attrs->{attachment} );
    Foswiki::Contrib::DBIStoreContrib::commit();
}

1;
