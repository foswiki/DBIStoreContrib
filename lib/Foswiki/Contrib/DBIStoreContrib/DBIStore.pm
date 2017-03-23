# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::DBIStore;

=begin TML

---+ package Foswiki::Contrib::DBIStoreContrib::DBIStore;

For Foswiki 1.2.0 and later, it's a piggyback implementation of the Store
class that will be inserted into the object hierarchy at object creation
time (a shim), to intercept store events using recordChange and cache the
results.

For Foswiki <1.2.0 (which does not support recordChange in the form
necessary to invoke the handler here) we use the DBIStorePlugin.

This module is not used when the store is entirely implemented using
DBIStoreContrib - this is only for when it is used as a cache.

=cut

use strict;
use warnings;

use Assert;
use Encode;

use Foswiki::Meta ();
use Foswiki::Contrib::DBIStoreContrib qw(%TRACE trace insert remove);

# @ISA not used directly, as it's set by the processing of the {ImplementationClasses}
# in 1.2+, and is irrelevant to 1.1-
# our @ISA = ('Foswiki::Store::Store');

# Constructor used when the object participates in a recordChange chain
# in a 1.2+ store.
sub new {
    my $class = shift;
    return $class->SUPER::new(@_);
}

sub DESTROY {
    my $this = shift;
    Foswiki::Contrib::DBIStoreContrib::disconnect();
}

=begin TML

---++ ObjectMethod recordChange(%args)
Record that the store item changed, and who changed it

Only called in Foswiki 1.2 and later. Prior to that, the plugin handlers
are used to trigger updates.

=cut

sub recordChange {
    my ( $this, %args ) = @_;

    # doing it first to make sure the record is chained
    $this->SUPER::recordChange(%args);

    # TODO: I'm not doing attachments yet (or maybe ever)
    return if ( defined( $args{newattachment} ) );
    return if ( defined( $args{oldattachment} ) );

    trace( $args{verb} . join( ',', keys(%args) ) ) if $TRACE{updates};

    if ( $args{verb} eq 'remove' ) {
        remove( $args{oldmeta}, $args{oldattachment} );
    }
    elsif ( $args{verb} eq 'insert' ) {
        insert( $args{newmeta}, $args{newattachment} );
    }
    elsif ( $args{verb} eq 'update' ) {
        remove( $args{oldmeta}, $args{oldAttachment} );
        if ( $args{newmeta} ) {
            insert( $args{newmeta}, $args{newattachment} );
        }
        else {
            insert( $args{oldmeta}, $args{oldattachment} );
        }
    }
}

=begin TML

---++ StaticMethod DBI_query( $sessio, $sql )
STATIC method invoked by Foswiki::Store::QueryAlgorithms::DBIStoreContrib
to perform the actual database query.

=cut

sub DBI_query {
    my ( $session, $sql ) = @_;

    my @names;
    eval {
        my $sth = Foswiki::Contrib::DBIStoreContrib::query($sql);
        while ( my @row = $sth->fetchrow_array() ) {
            push( @names, "$row[0]/$row[1]" );
        }
    };
    if ($@) {
        trace($@) if $TRACE{updates};
        die $@;
    }
    trace( 'HITS: ', scalar(@names), map { "\t$_" } @names )
      if $TRACE{updates};
    return \@names;
}

1;
__DATA__

Author: Crawford Currie http://c-dot.co.uk

Module of Foswiki - The Free and Open Source Wiki, http://foswiki.org/, http://Foswiki.org/

Copyright (C) 2010-2014 Foswiki Contributors. All Rights Reserved.
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

