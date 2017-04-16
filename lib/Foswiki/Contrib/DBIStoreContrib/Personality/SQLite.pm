# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::Personality::SQLite;

# DBIStoreContrib personality module for SQLite

use strict;
use warnings;
use Foswiki::Contrib::DBIStoreContrib qw(trace %TRACE $TABLE_PREFIX);
use Encode ();

use Foswiki::Contrib::DBIStoreContrib::Personality ();
our @ISA = ('Foswiki::Contrib::DBIStoreContrib::Personality');

# Use the database version this has been tested with
our $VERSION = '3.14';

sub new {
    my ( $class, $dbistore ) = @_;
    my $this = $class->SUPER::new($dbistore);
    $this->reserve(
        qw/
          ABORT ACTION AFTER ANALYZE ATTACH AUTOINCREMENT BEFORE BEGIN CAST
          COMMIT CONFLICT COVERING DATABASE DATETIME DEFERRABLE DEFERRED DETACH
          EACH END ESCAPE EXCEPT EXCLUSIVE EXPLAIN FAIL FULL GLOB IF IGNORE
          IMMEDIATE INDEXED INITIALLY INSTEAD INTERSECT ISNULL LIMIT MATCH
          NATURAL NO NOTNULL OF OFFSET PLAN PRAGMA QUERY RAISE REGEXP REINDEX
          RELEASE RENAME REPLACE ROLLBACK ROW SAVEPOINT TEMP TEMPORARY
          TRANSACTION TRIGGER USING VACUUM VIEW VIRTUAL WITHOUT/
    );

    return $this;
}

sub startup {
    my ( $this, $dbh ) = @_;
    $this->SUPER::startup($dbh);

    # Load PCRE, required for regexes
    $this->{dbh}->sqlite_enable_load_extension(1);
    $this->sql( "SELECT load_extension('"
          . $Foswiki::cfg{Extensions}{DBIStoreContrib}{SQLite}{PCRE}
          . "')" );
}

# Driver handles unicode, no need to encode on call. However it
# requires results to be decoded.
sub from_db { return Encode::decode_utf8( $_[1] ); }

sub table_exists {
    my $this = shift;
    my $phs  = join( ',', map { '?' } @_ );
    my $sth  = $this->sql(
        "SELECT name FROM sqlite_master WHERE type='table'"
          . " AND name IN ($phs)",
        map { $TABLE_PREFIX . $_ } @_
    );
    my $rows = $sth->fetchall_arrayref();
    return scalar(@$rows) == scalar(@_);
}

sub get_columns {
    my ( $this, $table, $column ) = @_;
    my $tn   = $TABLE_PREFIX . $table;
    my $sth  = $this->sql("PRAGMA table_info($tn)");
    my $rows = $sth->fetchall_arrayref();
    return { map { $this->from_db( $_->[1] ) => $this->from_db( $_->[2] ) }
          @$rows };
}

sub regexp {
    my ( $this, $sexpr, $pat ) = @_;

    unless ( $pat =~ s/^'(.*)'$/$1/s ) {

        # Somewhat risky....
        return "$sexpr REGEXP $pat";
    }

    # The macro parser does horrible things with \, causing \\
    # to become \\\. Force it back to \\
    $pat =~ s/\\{3}/\\\\/g;

    # SQLite uses PCRE, which supports all of Perl except hex
    # char codes
    $pat =~ s/\\x([0-9a-f]{2})/_char("0x$1")/gei;
    $pat =~ s/\\x\{([0-9a-f]+)\}/_char("0x$1")/gei;

    # Escape '
    $pat =~ s/'/\\'/g;

    return "$sexpr REGEXP '$pat'";
}

1;

__DATA__

Author: Crawford Currie http://c-dot.co.uk

Module of Foswiki - The Free and Open Source Wiki, http://foswiki.org/, http://Foswiki.org/

Copyright (C) 2013-2014 Foswiki Contributors. All Rights Reserved.
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
