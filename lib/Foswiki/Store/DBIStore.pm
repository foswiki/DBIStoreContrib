# See bottom of file for license and copyright information

=begin TML

---+ package Foswiki::Store::DBIStore

   * There are no webs, only topics (and attachments)
   * Attachments are stored in special table
   * Topic histories are stored in the same way as topics
   * A new rev of a topic creates a new tid. The latest rev of a topic
     is always marked so we know if it's an 'interesting' rev

=cut

package Foswiki::Store::DBIStore;
use strict;
use warnings;
use Fcntl qw( :flock );

use Foswiki::Store ();
our @ISA = ('Foswiki::Store');

use Assert;
use Error qw( :try );

use Foswiki                                     ();
use Foswiki::Meta                               ();
use Foswiki::Sandbox                            ();
use Foswiki::Iterator::NumberRangeIterator      ();
use Foswiki::Users::BaseUserMapping             ();
use Foswiki::Contrib::DBIStoreContrib::DBIStore ();
use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace personality);

my $wptn = "/$Foswiki::cfg{WebPrefsTopicName}.txt";

BEGIN {

    # Import the locale for sorting
    if ( $Foswiki::cfg{UseLocale} ) {
        require locale;
        import locale();
    }
}

sub new {
    my $class = shift;
    my $this  = $class->SUPER::new(@_);

    return $this;
}

sub finish {
    my $this = shift;
    $this->SUPER::finish();
    undef $this->{queryObj};
    undef $this->{searchQueryObj};
}

# Implement Foswiki::Store
sub readTopic {
    my ( $this, $meta, $version ) = @_;

    # check that the requested revision actually exists
    my @revs = ();
    my $nr = _numRevisions( \@revs, $meta );
    if ( defined $version && $version =~ /^\d+$/ ) {
        $version = $nr if ( $version == 0 || $version > $nr );
    }
    else {
        undef $version;

        # if it's a non-numeric string, we need to return undef
        # "...$version is defined but refers to a version that does
        # not exist, then $rev is undef"
    }

    my ( $text, $isLatest ) = _getRevision( \@revs, $meta, undef, $version );

    unless ( defined $text ) {
        ASSERT( not $isLatest ) if DEBUG;
        $meta->setLoadStatus( undef, $isLatest );
        return ( undef, $isLatest );
    }

    $text =~ s/\r//g;    # Remove carriage returns
                         # Parse meta-data out of the text
    $meta->setEmbeddedStoreForm($text);

    $version = $isLatest ? $nr : $version;

    # Patch up the revision info with defaults.
    my %ri;

    # The history metafile
    my $mf = _metaFile( $meta, undef, $version );
    ( $ri{author}, $ri{comment} ) = _readMetaFile($mf);
    $ri{date} = ( stat _historyFile( $meta, undef, $version ) )[9];
    $ri{comment} = '' unless defined $ri{comment};
    $ri{author} ||= $Foswiki::Users::BaseUserMapping::UNKNOWN_USER_CUID;

    $meta->setRevisionInfo(%ri);

    # If there is a history, but the latest version of the topic
    # is out-of-date, then the author must be unknown to reflect
    # what happens on checking

    $meta->setLoadStatus( $version, $isLatest );
    return ( $version, $isLatest );
}

# Implement Foswiki::Store
sub moveAttachment {
    my ( $this, $oldTopicObject, $oldAttachment, $newTopicObject,
        $newAttachment, $cUID )
      = @_;

    # No need to save damage; we're not looking inside

    my $oldLatest = _latestFile( $oldTopicObject, $oldAttachment );
    if ( -e $oldLatest ) {
        my $newLatest = _latestFile( $newTopicObject, $newAttachment );
        _moveFile( $oldLatest, $newLatest );
        _moveFile(
            _historyDir( $oldTopicObject, $oldAttachment ),
            _historyDir( $newTopicObject, $newAttachment )
        );

        $this->recordChange(
            _meta         => $oldTopicObject,
            cuid          => $cUID,
            revision      => 0,
            verb          => 'update',
            oldmeta       => $oldTopicObject,
            oldattachment => $oldAttachment,
            newmeta       => $newTopicObject,
            newattachment => $newAttachment
        );
    }
}

# Implement Foswiki::Store
sub copyAttachment {
    my ( $this, $oldTopicObject, $oldAttachment, $newTopicObject,
        $newAttachment, $cUID )
      = @_;

    # No need to save damage; we're not looking inside

    my $oldbase = _getPub($oldTopicObject);
    if ( -e "$oldbase/$oldAttachment" ) {
        my $newbase = _getPub($newTopicObject);
        _copyFile(
            _latestFile( $oldTopicObject, $oldAttachment ),
            _latestFile( $newTopicObject, $newAttachment )
        );
        _copyFile(
            _historyDir( $oldTopicObject, $oldAttachment ),
            _historyDir( $newTopicObject, $newAttachment )
        );

        $this->recordChange(
            _meta         => $oldTopicObject,
            cuid          => $cUID,
            revision      => 0,
            verb          => 'insert',
            newmeta       => $newTopicObject,
            newattachment => $newAttachment
        );
    }
}

# Implement Foswiki::Store
sub attachmentExists {
    my ( $this, $meta, $att ) = @_;

    # No need to save damage; we're not looking inside
    return -e _latestFile( $meta, $att )
      || -e _historyFile( $meta, $att );
}

# Implement Foswiki::Store
sub moveTopic {
    my ( $this, $oldTopicObject, $newTopicObject, $cUID ) = @_;

    my @revs;
    my $rev = _numRevisions( \@revs, $oldTopicObject );

    _moveFile( _latestFile($oldTopicObject), _latestFile($newTopicObject) );
    _moveFile( _historyDir($oldTopicObject), _historyDir($newTopicObject) );
    my $pub = _getPub($oldTopicObject);
    if ( -e $pub ) {
        _moveFile( $pub, _getPub($newTopicObject) );
    }

    if ( $newTopicObject->web ne $oldTopicObject->web ) {

        # Record that it was moved away
        $this->recordChange(
            _meta    => $oldTopicObject,
            cuid     => $cUID,
            revision => $rev,
            verb     => 'update',
            oldmeta  => $oldTopicObject,
            newmeta  => $newTopicObject
        );
    }

    $this->recordChange(
        _meta    => $newTopicObject,
        cuid     => $cUID,
        revision => $rev,
        verb     => 'update',
        oldmeta  => $oldTopicObject,
        newmeta  => $newTopicObject
    );
}

# Implement Foswiki::Store
sub moveWeb {
    my ( $this, $oldWebObject, $newWebObject, $cUID ) = @_;

    # No need to save damage; we're not looking inside

    my $oldbase = _getData($oldWebObject);
    my $newbase = _getData($newWebObject);

    _moveFile( $oldbase, $newbase );

    $oldbase = _getPub($oldWebObject);
    if ( -e $oldbase ) {
        $newbase = _getPub($newWebObject);

        _moveFile( $oldbase, $newbase );
    }

    # We have to log in the new web, otherwise we would re-create the dir with
    # a useless .changes. See Item9278
    $this->recordChange(
        _meta    => $newWebObject,
        cuid     => $cUID,
        revision => 0,
        more     => 'Moved from ' . $oldWebObject->web,
        verb     => 'update',
        oldmeta  => $oldWebObject,
        newmeta  => $newWebObject
    );
}

# Implement Foswiki::Store
sub testAttachment {
    my ( $this, $meta, $attachment, $test ) = @_;
    my $fn = _latestFile( $meta, $attachment );
    return eval "-$test '$fn'";
}

# Implement Foswiki::Store
sub openAttachment {
    my ( $this, $meta, $att, $mode, @opts ) = @_;
    return _openStream( $meta, $att, $mode, @opts );
}

# Implement Foswiki::Store
sub getRevisionHistory {
    my ( $this, $meta, $attachment ) = @_;

    unless ( -e _historyDir( $meta, $attachment ) ) {
        my @list = ();
        require Foswiki::ListIterator;
        if ( -e _latestFile( $meta, $attachment ) ) {
            push( @list, 1 );
        }
        return Foswiki::ListIterator->new( \@list );
    }
    my @revs;
    my $n = _numRevisions( \@revs, $meta, $attachment );

    return Foswiki::Iterator::NumberRangeIterator->new( $n, 1 );
}

# Implement Foswiki::Store
sub getNextRevision {
    my ( $this, $meta ) = @_;

    my @revs;
    return _numRevisions( \@revs, $meta ) + 1;
}

# Implement Foswiki::Store
sub getRevisionDiff {
    my ( $this, $meta, $rev2, $contextLines ) = @_;

    my $rev1 = $meta->getLoadedRev();
    my @list;
    my @revs;
    my ($text1) = _getRevision( \@revs, $meta, undef, $rev1 );
    my ($text2) = _getRevision( \@revs, $meta, undef, $rev2 );

    my $lNew = _split($text1);
    my $lOld = _split($text2);
    require Algorithm::Diff;
    my $diff = Algorithm::Diff::sdiff( $lNew, $lOld );

    foreach my $ele (@$diff) {
        push @list, $ele;
    }
    return \@list;
}

# Implement Foswiki::Store
sub getVersionInfo {
    my ( $this, $meta, $rev, $attachment ) = @_;

    my $df;
    my @revs;
    my $nr = _numRevisions( \@revs, $meta, $attachment );
    my $is_latest = 0;
    if ( $rev && $rev > 0 && $rev < $nr ) {
        $df = _historyFile( $meta, $attachment, $rev );
        unless ( -e $df ) {

            # May arise if the history is not continuous, or if
            # there is no history
            $df        = _latestFile( $meta, $attachment );
            $rev       = $nr;
            $is_latest = 1;
        }
    }
    else {
        $df        = _latestFile( $meta, $attachment );
        $rev       = $nr;
        $is_latest = 1;
    }
    my $info = {};

    # We can trust the history metafile
    my $mf = _metaFile( $meta, $attachment, $rev );
    ( $info->{author}, $info->{comment} ) = _readMetaFile($mf);

    $info->{date} ||= _getTimestamp($df);
    $info->{version} = $rev;
    $info->{comment} = '' unless defined $info->{comment};
    $info->{author} ||= $Foswiki::Users::BaseUserMapping::UNKNOWN_USER_CUID;

    return $info;
}

# Implement Foswiki::Store
sub saveAttachment {

    # SMELL: $options not currently supported by the core
    my ( $this, $meta, $name, $stream, $cUID, $options ) = @_;

    my @revs;
    my $rn = _numRevisions( \@revs, $meta, $name ) + 1;
    my $verb = ( $meta->hasAttachment($name) ) ? 'update' : 'insert';

    my $latest = _latestFile( $meta, $name );
    _saveStream( $latest, $stream );
    my $hf = _historyFile( $meta, $name, $rn );
    _mkPathTo($hf);
    File::Copy::copy( $latest, $hf )
      or die "DBIStore: failed to copy $latest to $hf: $!";

    my $comment;
    if ( ref $options ) {
        if ( $options->{forcedate} ) {
            utime( $options->{forcedate}, $options->{forcedate},
                $latest )    # touch
              or die "DBIStore: could not touch $latest: $!";
            utime( $options->{forcedate}, $options->{forcedate}, $hf )
              or die "DBIStore: could not touch $hf: $!";
        }
        $comment = $options->{comment};
    }
    else {

        # Compatibility with old signature
        $comment = $options;
        $options = {};
    }

    my $mf = _metaFile( $meta, $name, $rn );
    _writeMetaFile( $mf, $cUID, $comment );

    $this->recordChange(
        _meta         => $meta,
        cuid          => $cUID,
        revision      => $rn,
        more          => $options->{minor} ? 'minor' : undef,
        verb          => $verb,
        newmeta       => $meta,
        newattachment => $name
    );

    return $rn;
}

# Implement Foswiki::Store
sub saveTopic {
    my ( $this, $meta, $cUID, $options ) = @_;

    my $verb = ( -e _latestFile($meta) ) ? 'update' : 'insert';
    my @revs;
    my $rn = _numRevisions( \@revs, $meta ) + 1;

    # Fix TOPICINFO
    my $ti = $meta->get('TOPICINFO');
    $ti->{version} = $rn;
    $ti->{date}    = $options->{forcedate} || time;
    $ti->{author}  = $cUID;

    # Create new latest
    my $latest = _latestFile($meta);
    _saveFile( $latest, $meta->getEmbeddedStoreForm() );

    # Create history file by copying latest (modification date
    # doesn't matter, so long as it's >= $latest)
    my $hf = _historyFile( $meta, undef, $rn );
    _mkPathTo($hf);
    File::Copy::copy( $latest, $hf )
      or die "DBIStore: failed to copy $latest to $hf: $!";
    if ( $options->{forcedate} ) {
        utime( $options->{forcedate}, $options->{forcedate}, $latest )   # touch
          or die "DBIStore: could not touch $latest: $!";
        utime( $options->{forcedate}, $options->{forcedate}, $hf )       # touch
          or die "DBIStore: could not touch $hf: $!";
    }

    my $mf = _metaFile( $meta, undef, $rn );
    _writeMetaFile( $mf, $cUID, $options->{comment} );

    my $extra = $options->{minor} ? 'minor' : '';

    $this->recordChange(
        _meta    => $meta,
        cuid     => $cUID,
        revision => $rn,
        more     => $extra,
        verb     => $verb,
        newmeta  => $meta
    );

    return $rn;
}

# Implement Foswiki::Store
sub repRev {
    my ( $this, $meta, $cUID, %options ) = @_;

    my @revs;
    my $rn = _numRevisions( \@revs, $meta );
    ASSERT( $rn, $meta->getPath ) if DEBUG;
    my $latest = _latestFile($meta);
    my $hf     = _historyFile( $meta, undef, $rn );
    my $t      = ( stat $latest )[9];                 # SMELL: use TOPICINFO?
    unlink($hf);

    my $ti = $meta->get('TOPICINFO');
    $ti->{version} = $rn;
    $ti->{date}    = $options{forcedate} || time;
    $ti->{author}  = $cUID;

    _saveFile( $latest, $meta->getEmbeddedStoreForm() );

    _mkPathTo($hf);
    File::Copy::copy( $latest, $hf )
      or die "DBIStore: failed to copy $latest to $hf: $!";
    my $mf = _metaFile( $meta, undef, $rn );
    _writeMetaFile( $mf, $cUID, $options{comment} );

    if ( $options{forcedate} ) {
        utime( $options{forcedate}, $options{forcedate}, $latest )    # touch
          or die "DBIStore: could not touch $latest: $!";
        utime( $options{forcedate}, $options{forcedate}, $hf )
          or die "DBIStore: could not touch $hf: $!";
    }

    my @log = ( 'minor', 'reprev', $options{operation} || 'save' );

    $this->recordChange(
        _meta    => $meta,
        cuid     => $cUID,
        revision => $rn,
        more     => join( ', ', @log ),
        verb     => 'update',
        newmeta  => $meta
    );

    return $rn;
}

# Implement Foswiki::Store
sub delRev {
    my ( $this, $meta, $cUID ) = @_;

    my @revs;
    my $rev = _numRevisions( \@revs, $meta );
    if ( $rev <= 1 ) {
        die 'DBIStore: Cannot delete initial revision of '
          . $meta->web . '.'
          . $meta->topic;
    }

    my $hf = _historyFile( $meta, undef, $rev );
    unlink $hf;

    # Get the new top rev - which may or may not be -1, depending if
    # the history is complete or not
    @revs = ();
    my $cur = _numRevisions( \@revs, $meta );
    $hf = _historyFile( $meta, undef, $cur );
    my $thf = _latestFile($meta);

    # Copy it up to the latest file, then refresh the time on the history
    File::Copy::copy( $hf, $thf )
      or die "DBIStore: failed to copy to $thf: $!";
    utime( undef, undef, $hf )    # touch
      or die "DBIStore: could not touch $hf: $!";

    # reload the topic object
    $meta->unload();
    $meta->loadVersion();

    $this->recordChange(
        _meta    => $meta,
        cuid     => $cUID,
        revision => $rev,
        verb     => 'update',
        newmeta  => $meta
    );

    return $rev;
}

# Implement Foswiki::Store
sub atomicLockInfo {
    my ( $this, $meta ) = @_;
    my $filename = _getData($meta) . '.lock';
    if ( -e $filename ) {
        my $t = _readFile($filename);
        return split( /\s+/, $t, 2 );
    }
    return ( undef, undef );
}

# It would be nice to use flock to do this, but the API is unreliable
# (doesn't work on all platforms)
sub atomicLock {
    my ( $this, $meta, $cUID ) = @_;
    my $filename = _getData($meta) . '.lock';
    _saveFile( $filename, $cUID . "\n" . time );
}

# Implement Foswiki::Store
sub atomicUnlock {
    my ( $this, $meta, $cUID ) = @_;

    my $filename = _getData($meta) . '.lock';
    unlink $filename
      or die "DBIStore: failed to delete $filename: $!";
}

# Implement Foswiki::Store
sub webExists {
    my ( $this, $web ) = @_;

    return 0 unless defined $web;
    $web =~ s#\.#/#go;

    return -e _latestFile( $web, $Foswiki::cfg{WebPrefsTopicName} );
}

# Implement Foswiki::Store
sub topicExists {
    my ( $this, $web, $topic ) = @_;

    return 0 unless defined $web && $web ne '';
    $web =~ s#\.#/#go;
    return 0 unless defined $topic && $topic ne '';

    return -e _latestFile( $web, $topic )
      || -e _historyDir( $web, $topic );
}

# Implement Foswiki::Store
sub getApproxRevTime {
    my ( $this, $web, $topic ) = @_;

    return ( stat( _latestFile( $web, $topic ) ) )[9] || 0;
}

# Implement Foswiki::Store
sub eachChange {
    my ( $this, $meta, $since ) = @_;

    my $file = _getData( $meta->web ) . '/.changes';
    require Foswiki::ListIterator;

    if ( -r $file ) {

        # Could use a LineIterator to avoid reading the whole
        # file, but it hardly seems worth it.
        my @changes =
          map {

            # Create a hash for this line
            {
                topic => Foswiki::Sandbox::untaint(
                    $_->[0], \&Foswiki::Sandbox::validateTopicName
                ),
                user     => $_->[1],
                time     => $_->[2],
                revision => $_->[3],
                more     => $_->[4]
            };
          }
          grep {

            # Filter on time
            $_->[2] && $_->[2] >= $since
          }
          map {

            # Split line into an array
            my @row = split( /\t/, $_, 5 );
            \@row;
          }
          reverse split( /[\r\n]+/, _readFile($file) );

        return Foswiki::ListIterator->new( \@changes );
    }
    else {
        my $changes = [];
        return Foswiki::ListIterator->new($changes);
    }
}

# Implement Foswiki::Store
# An attachment is only an attachment if it has a presence in the meta-data
sub eachAttachment {
    my ( $this, $meta ) = @_;

    my $dh;
    opendir( $dh, _attachmentsDir($meta) )
      or return new Foswiki::ListIterator( [] );
    my @list = grep { !/^[.*_]/ && !/,pfv$/ } readdir($dh);
    closedir($dh);

    require Foswiki::ListIterator;
    return new Foswiki::ListIterator( \@list );
}

# Implement Foswiki::Store
sub eachTopic {
    my ( $this, $meta ) = @_;

    my $dh;
    opendir( $dh, _getData( $meta->web ) )
      or return ();

    # the name filter is used to ensure we don't return filenames
    # that contain illegal characters as topic names.
    my @list =
      map { /^(.*)\.txt$/; $1; }
      sort
      grep { !/$Foswiki::cfg{NameFilter}/ && /\.txt$/ } readdir($dh);
    closedir($dh);

    require Foswiki::ListIterator;
    return new Foswiki::ListIterator( \@list );
}

# Implement Foswiki::Store
sub eachWeb {
    my ( $this, $meta, $all ) = @_;

    # Undocumented; this fn actually accepts a web name as well. This is
    # to make the recursion more efficient.
    my $web = ref($meta) ? $meta->web : $meta;

    my $dir = $Foswiki::cfg{DataDir};
    $dir .= '/' . $web if defined $web;
    my @list;
    my $dh;

    if ( opendir( $dh, $dir ) ) {
        @list = map {

            # Tradeoff: correct validation of every web name, which allows
            # non-web directories to be interleaved, thus:
            #    Foswiki::Sandbox::untaint(
            #           $_, \&Foswiki::Sandbox::validateWebName )
            # versus a simple untaint, much better performance:
            Foswiki::Sandbox::untaintUnchecked($_)
          }

          # The -e on the web preferences is used in preference to a
          # -d to avoid having to validate the web name each time. Since
          # the definition of a Web in this handler is "a directory with a
          # WebPreferences.txt in it", this works.
          grep { !/\./ && -e "$dir/$_$wptn" } readdir($dh);
        closedir($dh);
    }

    if ($all) {
        my $root = $web ? "$web/" : '';
        my @expandedList;
        while ( my $wp = shift(@list) ) {
            push( @expandedList, $wp );
            my $it = $this->eachWeb( $root . $wp, $all );
            push( @expandedList, map { "$wp/$_" } $it->all() );
        }
        @list = @expandedList;
    }
    @list = sort(@list);
    require Foswiki::ListIterator;
    return new Foswiki::ListIterator( \@list );
}

# Implement Foswiki::Store
sub remove {
    my ( $this, $cUID, $meta, $attachment ) = @_;
    my $f;
    if ( $meta->topic ) {

        # Topic or attachment
        unlink( _latestFile( $meta, $attachment ) );
        _rmtree( _historyDir( $meta, $attachment ) );
        _rmtree( _getPub($meta) ) unless ($attachment);    # topic only
    }
    else {

        # Web
        _rmtree( _getData($meta) );
        _rmtree( _getPub($meta) );
    }

    # Only log when deleting topics or attachment, otherwise we would re-create
    # an empty directory with just a .changes.
    if ($attachment) {
        $this->recordChange(
            _meta         => $meta,
            cuid          => $cUID,
            revision      => 0,
            more          => 'Deleted attachment ' . $attachment,
            verb          => 'remove',
            oldmeta       => $meta,
            oldattachment => $attachment
        );
    }
    elsif ( my $topic = $meta->topic ) {
        $this->recordChange(
            _meta         => $meta,
            cuid          => $cUID,
            revision      => $0,
            more          => 'Deleted ' . $topic,
            verb          => 'remove',
            oldmeta       => $meta,
            oldattachment => $attachment
        );
    }
}

# Implement Foswiki::Store
sub query {
    my ( $this, $query, $inputTopicSet, $session, $options ) = @_;

    my $engine;
    if ( $query->isa('Foswiki::Query::Node') ) {
        unless ( $this->{queryObj} ) {
            my $module = $Foswiki::cfg{Store}{QueryAlgorithm};
            eval "require $module";
            die
"Bad {Store}{QueryAlgorithm}; suggest you run configure and select a different algorithm\n$@"
              if $@;
            $this->{queryObj} = $module->new();
        }
        $engine = $this->{queryObj};
    }
    else {
        ASSERT( $query->isa('Foswiki::Search::Node') ) if DEBUG;
        unless ( $this->{searchQueryObj} ) {
            my $module = $Foswiki::cfg{Store}{SearchAlgorithm};
            eval "require $module";
            die
"Bad {Store}{SearchAlgorithm}; suggest you run configure and select a different algorithm\n$@"
              if $@;
            $this->{searchQueryObj} = $module->new();
        }
        $engine = $this->{searchQueryObj};
    }

    no strict 'refs';
    return $engine->query( $query, $inputTopicSet, $session, $options );
    use strict 'refs';
}

# Implement Foswiki::Store
sub getRevisionAtTime {
    my ( $this, $meta, $time ) = @_;

    my $hd = _historyDir($meta);
    my $d;
    unless ( opendir( $d, $hd ) ) {
        return 1 if ( $time >= ( stat( _latestFile($meta) ) )[9] );
        return undef;
    }
    my @revs;
    _loadRevs( \@revs, $hd );

    foreach my $rev ( reverse @revs ) {
        return $rev if ( $time >= ( stat("$hd/$rev") )[9] );
    }
    return undef;
}

# Implement Foswiki::Store
sub getLease {
    my ( $this, $meta ) = @_;

    my $filename = _getData($meta) . '.lease';
    my $lease;
    if ( -e $filename ) {
        my $t = _readFile($filename);
        $lease = { split( /\r?\n/, $t ) };
    }
    return $lease;
}

# Implement Foswiki::Store
sub setLease {
    my ( $this, $meta, $lease ) = @_;

    my $filename = _getData($meta) . '.lease';
    if ($lease) {
        _saveFile( $filename, join( "\n", %$lease ) );
    }
    elsif ( -e $filename ) {
        unlink $filename
          or die "DBIStore: failed to delete $filename: $!";
    }
}

# Implement Foswiki::Store
sub removeSpuriousLeases {
    my ( $this, $web ) = @_;
    my $webdir = _getData($web) . '/';
    if ( opendir( my $W, $webdir ) ) {
        foreach my $f ( readdir($W) ) {
            my $file = $webdir . $f;
            if ( $file =~ /^(.*)\.lease$/ ) {
                if ( !-e "$1,pfv" ) {
                    unlink($file);
                }
            }
        }
        closedir($W);
    }
}

#############################################################################
# PRIVATE FUNCTIONS
#############################################################################

# Get the absolute file path to a file in data. $what can be a Meta or
# a string path (e.g. a web name)
sub _getData {
    my ($what) = @_;
    my $path = $Foswiki::cfg{DataDir} . '/';
    return $path . $what unless ref($what);
    return $path . $what->web unless $what->topic;
    return $path . $what->web . '/' . $what->topic;
}

# Get the absolute file path to a file in pub. $what can be a Meta or
# a string path (e.g. a web name)
sub _getPub {
    my ($what) = @_;
    my $path = $Foswiki::cfg{PubDir} . '/';
    return $path . $what unless ref($what);
    return $path . $what->web unless $what->topic;
    return $path . $what->web . '/' . $what->topic;
}

# Load an array of the revisions stored in the given directory, sorted
# most recent (highest numbered) revision first.
sub _loadRevs {
    my ( $revs, $dir ) = @_;
    my $d;
    opendir( $d, $dir ) or die "DBIStore: '$dir': $!";

    # Read, untaint, sort in reverse
    @$revs = sort { $b <=> $a }
      map { /([0-9]+)/; $1 } grep { /^[0-9]+$/ } readdir($d);
    closedir($d);
}

# Get the absolute file path to the latest version of a topic or attachment
# _latestFile($meta [, $attachment])
#    - $meta is a Foswiki::Meta
# _latestFile( $web, $topic [, $attachment])
#    - web and topic are strings
sub _latestFile {
    my $p1 = shift;
    my $p2 = shift;

    unless ( ref($p1) ) {
        $p1 = "$p1/$p2";
        $p2 = shift;
    }
    return _getPub($p1) . "/$p2" if $p2;
    return _getData($p1) . ".txt";
}

# Get the absolute file path to the attachments metadir for a topic
sub _attachmentsDir {
    return _getData( $_[0] ) . ',pfv/ATTACHMENTS';
}

# Get the absolute file path to the history dir for a topic or attachment
# _historyDir($meta [, $attachment])
#    - $meta is a Foswiki::Meta
# _historyDir( $web, $topic [, $attachment])
#    - web and topic are strings
sub _historyDir {
    my $p1 = shift;
    my $p2 = shift;

    unless ( ref($p1) ) {
        $p1 = "$p1/$p2";
        $p2 = shift;
    }

    # $p1 is web/topic
    # $p2 is attachment name (if any)
    if ($p2) {

        # It's an attachment. The history is stored in the web data dir, in
        # a subdir with the same name as the topic and "extension" ,pfm
        # This keeps the pub directory "clean"; a requirement when these
        # files are visible via a web interface.
        return _attachmentsDir($p1) . "/${p2}";
    }
    else {

        # It's a topic. The history is stored in the web data dir.
        return _getData($p1) . ",pfv";
    }
}

# Get the absolute file path to the history for a topic or attachment
# _historyFile($meta, $attachment, $version)
#    - $meta is a Foswiki::Meta
# _historyFile( $web, $topic, $attachment, $version)
#    - web and topic are strings
sub _historyFile {
    my $ver = pop;
    return _historyDir(@_) . "/$ver";
}

# Get the absolute file path to the metafile for a topic or attachment
# _metaFile($meta, $attachment, $version)
#    - $meta is a Foswiki::Meta
# _metaFile( $web, $topic, $attachment, $version)
#    - web and topic are strings
sub _metaFile {
    return _historyFile(@_) . '.m';
}

# Get the number of revisions for a topic or attachment
sub _numRevisions {
    my ( $revs, $meta, $attachment ) = @_;
    my $sql;

    Foswiki::Contrib::DBIStoreContrib::getDBH();
    my $r;
    if ($attachment) {
        my $sql =
            "SELECT \"${TABLE_PREFIX}FILEATTACHMENT\".version"
          . " FROM \"FILEATTACHMENT\",\"TOPICINFO\",${TABLE_PREFIX}topic"
          . " WHERE "
          . join(
            ' AND ',

            #                   "NOT topic.\"isHistory\"",
            "${TABLE_PREFIX}topic.web='" . site2uc( $meta->web ) . "'",
            "${TABLE_PREFIX}topic.name='" . site2uc( $meta->topic ) . "'",
            "\"${TABLE_PREFIX}TOPICINFO\".tid=${TABLE_PREFIX}topic.tid",
            "\"${TABLE_PREFIX}FILEATTACHMENT\".tid=${TABLE_PREFIX}topic.tid"
          );
        $r = personality->sql( 'selectall_arrayref', $sql );
    }
    else {
        # Get all versions
        $sql =
            "SELECT \"${TABLE_PREFIX}TOPICINFO\".version"
          . " FROM ${TABLE_PREFIX}topic,\"${TABLE_PREFIX}TOPICINFO\""
          . " WHERE "
          . join(
            ' AND ',

            #                   "NOT topic.\"isHistory\"",
            "${TABLE_PREFIX}topic.web='" . site2uc( $meta->web ) . "'",
            "${TABLE_PREFIX}topic.name='" . site2uc( $meta->topic ) . "'",
            "\"${TABLE_PREFIX}TOPICINFO\".tid=${TABLE_PREFIX}topic.tid"
          );
        $r = personality->sql( 'selectall_arrayref', $sql );
    }
    my @r = reverse sort { $a->[0] <=> $b->[0] } @$r;

    return $r[0]->[0];
}

sub _readMetaFile {
    my $mf = shift;
    return () unless -e $mf;
    return split( "\n", _readFile($mf), 2 );
}

sub _writeMetaFile {
    my $mf = shift;
    _mkPathTo($mf);
    _saveFile( $mf, join( "\n", map { defined $_ ? $_ : '' } @_ ) );
}

# Record a change in the web history
sub recordChange {
    my $this = shift;
    my %args = @_;
    $args{more} ||= '';
    ASSERT( $args{cuid} ) if DEBUG;
    ASSERT( defined $args{more} ) if DEBUG;

    #    my ( $meta, $cUID, $rev, $more ) = @_;
    #    $more ||= '';

    my $file = _getData( $args{_meta}->web ) . '/.changes';
    my @changes;
    my $text = '';
    my $t    = time;

    if ( -e $file ) {
        my $cutoff = $t - $Foswiki::cfg{Store}{RememberChangesFor};
        my $fh;
        open( $fh, '<', $file )
          or die "DBIStore: failed to read $file: $!";
        local $/ = "\n";
        my $head = 1;
        while ( my $line = <$fh> ) {
            chomp($line);
            if ($head) {
                my @row = split( /\t/, $line, 4 );
                next if ( $row[2] < $cutoff );
                $head = 0;
            }
            $text .= "$line\n";
        }
        close($fh);
    }

    # Add the new change to the end of the file
    $text .= $args{_meta}->topic || '.';
    $text .= "\t$args{cuid}\t$t\t$args{revision}\t$args{more}\n";

    _saveFile( $file, $text );
}

# Read an entire file
sub _readFile {
    my ($name) = @_;

    my $data;
    my $IN_FILE;
    open( $IN_FILE, '<', $name )
      or die "DBIStore: failed to read $name: $!";
    binmode($IN_FILE);
    local $/ = undef;
    $data = <$IN_FILE>;
    close($IN_FILE);
    $data = '' unless defined $data;
    return $data;
}

# Open a stream onto a file
sub _openStream {
    my ( $meta, $att, $mode, %opts ) = @_;
    my $stream;

    my $path;
    my @revs;
    if (   $opts{version}
        && $opts{version} < _numRevisions( \@revs, $meta, $att ) )
    {
        ASSERT( $mode !~ />/ ) if DEBUG;
        $path = _historyFile( $meta, $att, $opts{version} );
    }
    else {
        $path = _latestFile( $meta, $att );
        _mkPathTo($path) if ( $mode =~ />/ );
    }
    unless ( open( $stream, $mode, $path ) ) {
        die("DBIStore: open stream $mode '$path' failed: $!");
    }
    binmode $stream;
    return $stream;
}

# Save a file
sub _saveFile {
    my ( $file, $text ) = @_;
    _mkPathTo($file);
    my $fh;
    open( $fh, '>', $file )
      or die("DBIStore: failed to create file $file: $!");
    flock( $fh, LOCK_EX )
      or die("DBIStore: failed to lock file $file: $!");
    binmode($fh)
      or die("DBIStore: failed to binmode $file: $!");
    print $fh $text
      or die("DBIStore: failed to print into $file: $!");
    close($fh)
      or die("DBIStore: failed to close file $file: $!");

    chmod( $Foswiki::cfg{Store}{filePermission}, $file );

    return;
}

# Save a stream to a file
sub _saveStream {
    my ( $file, $fh ) = @_;

    _mkPathTo($file);
    my $F;
    open( $F, '>', $file ) or die "DBIStore: open $file failed: $!";
    binmode($F) or die "DBIStore: failed to binmode $file: $!";
    my $text;
    while ( read( $fh, $text, 1024 ) ) {
        print $F $text;
    }
    close($F) or die "DBIStore: close $file failed: $!";

    chmod( $Foswiki::cfg{Store}{filePermission}, $file );
}

# Move a file or directory from one absolute file path to another.
# if the destination already exists it's an error.
sub _moveFile {
    my ( $from, $to ) = @_;
    die "DBIStore: move target $to already exists" if -e $to;
    _mkPathTo($to);
    my $ok;
    if ( -d $from ) {
        $ok = File::Copy::Recursive::dirmove( $from, $to );
    }
    else {
        ASSERT( -e $from, $from ) if DEBUG;
        $ok = File::Copy::move( $from, $to );
    }
    $ok or die "DBIStore: move $from to $to failed: $!";
}

# Copy a file or directory from one absolute file path to another.
# if the destination already exists it's an error.
sub _copyFile {
    my ( $from, $to ) = @_;

    die "DBIStore: move target $to already exists" if -e $to;
    _mkPathTo($to);
    my $ok;
    if ( -d $from ) {
        $ok = File::Copy::Recursive::dircopy( $from, $to );
    }
    else {
        $ok = File::Copy::copy( $from, $to );
    }
    $ok or die "DBIStore: copy $from to $to failed: $!";
}

# Make all directories above the path
sub _mkPathTo {
    my $file = shift;

    ASSERT( File::Spec->file_name_is_absolute($file) ) if DEBUG;

    my ( $volume, $path, undef ) = File::Spec->splitpath($file);
    $path = File::Spec->catpath( $volume, $path, '' );

    # SMELL:  Sites running Apache with SuexecUserGroup will
    # have a forced "safe" umask. Override umask here to allow
    # correct dirPermissions to be applied
    umask( oct(777) - $Foswiki::cfg{Store}{dirPermission} );

    eval {
        File::Path::mkpath( $path, 0, $Foswiki::cfg{Store}{dirPermission} );
    };
    if ($@) {
        die("DBIStore: failed to create ${path}: $!");
    }
}

# Remove an entire directory tree
sub _rmtree {
    my $root = shift;
    my $D;
    if ( opendir( $D, $root ) ) {
        foreach my $entry ( grep { !/^\.+$/ } readdir($D) ) {
            $entry =~ /^(.*)$/;
            $entry = $root . '/' . $1;
            if ( -d $entry ) {
                _rmtree($entry);
            }
            elsif ( !unlink($entry) && -e $entry ) {
                if ( $Foswiki::cfg{OS} ne 'WINDOWS' ) {
                    die "DBIStore: Failed to delete file $entry: $!";
                }
                else {

                    # Windows sometimes fails to delete files when
                    # subprocesses haven't exited yet, because the
                    # subprocess still has the file open. Live with it.
                    warn "WARNING: Failed to delete file $entry: $!";
                }
            }
        }
        closedir($D);

        if ( !rmdir($root) ) {
            if ( $Foswiki::cfg{OS} ne 'WINDOWS' ) {
                die "DBIStore: Failed to delete $root: $!";
            }
            else {
                warn "WARNING: Failed to delete $root: $!";
            }
        }
    }
}

# Get the timestamp on a file. 0 indicates the file was not found.
sub _getTimestamp {
    my ($file) = @_;

    my $date = 0;
    if ( -e $file ) {

        # If the stat fails, stamp it with some arbitrary static
        # time in the past (00:40:05 on 5th Jan 1989)
        $date = ( stat $file )[9] || 600000000;
    }
    return $date;
}

# Get a specific revision of a topic or attachment
sub _getRevision {
    my ( $revs, $meta, $attachment, $version ) = @_;

    my $nr = _numRevisions( $revs, $meta, $attachment );
    if ( $nr && $version && $version <= $nr ) {
        my $fn = _historyDir( $meta, $attachment ) . "/$version";
        if ( -e $fn ) {
            return ( _readFile($fn), $version == $nr );
        }
    }
    my $latest = _latestFile( $meta, $attachment );
    return ( undef, 0 ) unless -e $latest;

    # no version given, give latest (may not be checked in yet)
    return ( _readFile($latest), 1 );
}

# Split a string on \n making sure we have all newlines. If the string
# ends with \n there will be a '' at the end of the split.
sub _split {

    #my $text = shift;

    my @list = ();
    return \@list unless defined $_[0];

    my $nl = 1;
    foreach my $i ( split( /(\n)/o, $_[0] ) ) {
        if ( $i eq "\n" ) {
            push( @list, '' ) if $nl;
            $nl = 1;
        }
        else {
            push( @list, $i );
            $nl = 0;
        }
    }
    push( @list, '' ) if ($nl);

    return \@list;
}

1;
__END__
Foswiki - The Free and Open Source Wiki, http://foswiki.org/

Copyright (C) 2014 Crawford Currie http://c-dot.co.uk

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version. For
more details read LICENSE in the root of this distribution.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

As per the GPL, removal of this notice is prohibited.
