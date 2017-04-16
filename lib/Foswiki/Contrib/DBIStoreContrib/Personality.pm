# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::Personality;

use strict;
use warnings;
use Assert;
use Encode ();

# Import type constants and tracing
use Foswiki::Contrib::DBIStoreContrib qw(
  NAME NUMBER STRING UNKNOWN BOOLEAN SELECTOR VALUE TABLE PSEUDO_BOOL
  $TABLE_PREFIX expandIDs
  trace %TRACE traceSQL);

# We try to use the ANSI SQL standard as far as possible, for the most
# part different SQL DB implementations support it fairly well. However
# they all have nuances, and there are areas where the support is not
# consistent - most notably in regex support. To that end we have to
# have a database personality module, which provides these custom
# operations in a consistent way.

=begin TML

{true_value} and {true_type} For DB's that
don't support a true BOOLEAN type, the true_type can be PSEUDO_BOOL,
in which case boolean operations on the data will always use =1.

=cut

sub new {
    my ($class) = @_;
    my $this = bless(
        {
            dbh => undef,    # Set in startup

            # A DB with native BOOLEAN can use a simple boolean expression
            # here. Without BOOLEAN support a more convoluted route is
            # required.
            true_value => '1=1',

            # If the DB has a native BOOLEAN type this is BOOLEAN. If it
            # has to use a BIT value, this will be PSEUDO_BOOL.
            true_type => BOOLEAN
        },
        $class
    );

    # SQL reserved words. The following words are reserved in all of
    # PostgresSQL, MySQL, SQLite and T-SQL so provide a good
    # working basis. Personality modules should extend this list.
    $this->reserve(
        qw(
          ADD ALL ALTER AND AS ASC BETWEEN BY CASCADE CASE CHECK COLLATE COLUMN
          CONSTRAINT CREATE CROSS CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP
          DEFAULT DELETE DESC DISTINCT DROP ELSE EXISTS FOR FOREIGN FROM GROUP
          HAVING IN INDEX INNER INSERT INTO IS JOIN KEY LEFT LIKE NOT NULL ON OR
          ORDER OUTER PRIMARY REFERENCES RESTRICT RIGHT SELECT SET TABLE THEN TO
          UNION UNIQUE UPDATE VALUES WHEN WHERE WITH
          )
    );
    return $this;
}

=begin TML

---++ ObjectMethod startup($dbh)
Execute any SQL commands required to start the DB.
Subclasses must call superclass.

=cut

sub startup {
    my ( $this, $dbh ) = @_;
    ASSERT($dbh) if DEBUG;
    $this->{dbh} = $dbh;

}

# Protected - for use by subclasses only
# Register reserved words
sub reserve {
    my $this = shift;
    foreach (@_) {
        $this->{reserved}->{$_} = 1;
    }
}

=begin TML

---++ ObjectMethod sql($sql, ...) -> $sth

Prepare a unicode SQL string for passing to a DBI function, and
execute it returning the statement handle. Returns the statement
handle.
   * $sql is the (unicode) SQL
   * ... are any additional parameters

Parameters are bound using =DBI::bind_param=

=cut

sub sql {
    my $this = shift;
    my $sql  = expandIDs(shift);

    if ( $TRACE{sql} ) {

        #my @c = caller;
        #trace($c[1], ':', $c[2]);
        traceSQL( $sql, @_ );
    }

    my $sth;
    eval {
        $sth = $this->{dbh}->prepare( $this->to_db($sql) );
        my $p = 1;
        foreach my $arg (@_) {
            $sth->bind_param( $p++, $this->to_db($arg) );
        }
        $sth->execute();
    };
    if ($@) {
        require Carp;
        Carp::confess($@);
    }
    return $sth;
}

=begin TML

---++ ObjectMethod to_db($ucs) -> $ds

Encode a string for passing to a DBI function. Some drivers require
you to encode to a different charset, and some don't. Default is
to not encode.
   * $ucs - string to encode
Return the driver-encoded version of $ucs

=cut

sub to_db {
    return $_[1];
}

=begin TML

---++ ObjectMethod from_db($ds) -> $ucs

Decode a string returned from a DBI function. Some drivers require
you to decode from a different charset, and some don't. Default is
to not decode.
   * $ds - string in a result returned from a DBI call
Return the unicode version of $ds

=cut

sub from_db {
    return $_[1];
}

=begin TML

---++ ObjectMethod table_exists(table_name [, table_name]*) -> boolean
Determine if a table exists. All tables named in parameters
must exist. Note that table_name is the UNPREFIXED table name

=cut

sub table_exists {
    my $this = shift;
    ASSERT( $this->{dbh} ) if DEBUG;

    # MySQL, Postgresql, MS SQL Server
    my $phs = join( ',', map { '?' } @_ );
    my @p = (
        'SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE '
          . "TABLE_NAME IN ($phs)",
        map { $TABLE_PREFIX . $_ } @_
    );

    #traceSQL(@p) if $TRACE{sql};
    my $sth  = $this->sql(@p);
    my $rows = $sth->fetchall_arrayref();
    return scalar(@$rows) == scalar(@_);
}

=begin TML

---++ ObjectMethod get_columns(table_name) -> %cols
Get a map of column names to type for the given table
Note: table_name is unprefixed!

=cut

sub get_columns {
    my ( $this, $table ) = @_;
    ASSERT( $this->{dbh} ) if DEBUG;

    # MySQL, Postgresql, MS SQL Server
    my $sth = $this->sql(
        'SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS'
          . ' WHERE TABLE_NAME=?',
        $TABLE_PREFIX . $table
    );
    my $s = $sth->fetchall_arrayref();
    my $res =
      { map { ( $this->from_db( $_->[0] ) => $this->from_db( $_->[1] ) ) }
          @$s };
    return $res;
}

=begin TML

---++ ObjectMethod regexp($expr, $pat) -> $sql
Construct an SQL expression to execute the given regular expression
match.
  * =$expr= - string expression
  * =$pat= - Foswiki (perl) regular expression

=cut

sub regexp {
    my ( $this, $expr, $pat ) = @_;

    # Yeah, like any SQL DB is going to do this...
    return "$expr REGEXP $pat";
}

=begin TML

---++ ObjectMethod wildcard($lhs, $rhs) -> $sql
Construct an SQL expression that will match a Foswiki wildcard
name match.

Default is ANSI standard.
ANSI wildcards in LIKE are:
 _ (underscore)
 Any one character. For example, a_ matches ab and ac, but not a.
 % (percent)
 Any string of zero or more characters. For example, bl% matches
 bl and bla.
 []
 Any single character in the specified range or set. For example,
 T[oi]m matches Tom or Tim.
 [^]
 Any single character not in the specified range or set. For
 example, M[^c] matches Mb and Md, but not Mc.
Foswiki uses * wildcards, and separates alternatives with comma, so this
is easy to do.

The default implementation uses the regexp function to match.

Note: the string input to this function must have single quotes already
doubled up.

=cut

sub wildcard {
    my ( $this, $lhs, $rhs ) = @_;
    my @exprs;
    if ( $rhs =~ s/^'(.*)'$/$1/ ) {
        foreach my $spec ( split( /(?:,\s*|\|)/, $rhs ) ) {
            my $like   = 0;
            my $escape = '';
            if ( $spec =~ s/([%_])/\0$1/g ) {
                $spec =~ s/!/!!/g;
                $spec =~ s/\0/!/g;
                $like   = 1;
                $escape = ' ESCAPE \'!\'';
            }
            $like = 1 if $spec =~ s/\*/%/g;
            $like = 1 if $spec =~ s/\?/_/g;

            if ($like) {
                my $res = "$lhs LIKE '$spec'$escape";
                push( @exprs, $res );
            }
            else {
                push( @exprs, "$lhs='$spec'" );
            }
        }
    }
    return join( ' OR ', @exprs );
}

=begin TML

---++ ObjectMethod d2n($timestring) -> $isosecs
Convert a Foswiki time string to a number.
This implementation is for SQLite - there is no support in ANSI.

=cut

sub d2n {
    my ( $this, $arg ) = @_;

    return "CAST(strftime(\"%s\", $arg) AS FLOAT)";
}

=begin

---++ ObjectMethod length($s) -> $i
Calculate the character length of a string

=cut

sub length {
    my ( $this, $s ) = @_;
    return "LENGTH($s)";
}

=begin TML

---++ ObjectMethod cast_to_numeric($sql) -> $sql
Cast a datum to a numeric type for comparison

=cut

sub cast_to_numeric {
    my ( $this, $d ) = @_;
    return "CAST(($d) AS NUMERIC)";
}

=begin TML

---++ ObjectMethod is_true($type, $sql) -> $sql
Test if SQL computes to NULL. Ideally this will test if the SQL evaluates
to the empty string, NULL, boolean FALSE or a numeric value of zero.

=cut

sub is_true {
    my ( $this, $type, $sql ) = @_;
    if ( $type == NUMBER || $type == PSEUDO_BOOL ) {
        return "($sql)!=0";
    }
    elsif ( $type == STRING ) {

        # Trickier, as under perl semantics '', '0' and undef all evaluate
        # to false.
        return "($sql)!=''";
    }
    else {    # $type == BOOLEAN or UNKNOWN
        return $sql;
    }
}

=begin TML

---++ ObjectMethod cast_to_string($sql) -> $sql
Cast a datum to a character string type for comparison

=cut

sub cast_to_text {
    my ( $this, $d ) = @_;
    return
        "CAST(($d) AS "
      . $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{_DEFAULT}{type}
      . ')';
}

=begin TML

---++ ObjectMethod make_comment() -> $comment_string
Make a comment string

=cut

sub make_comment {
    my $this = shift;
    return '/*' . join( ' ', @_ ) . '*/';
}

=begin TML

---++ ObjectMethod strcat($str1 [$str2 [, ... strN]) -> $concatenated
Use the SQL string concatenation operator to concatente strings.

=cut

sub strcat {
    my $this = shift;
    return join( '||', @_ );
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
