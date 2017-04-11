# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::Personality;

use strict;
use warnings;
use Assert;
use Encode ();

# Import type constants
use Foswiki::Contrib::DBIStoreContrib qw(NAME NUMBER STRING UNKNOWN
  BOOLEAN SELECTOR VALUE TABLE PSEUDO_BOOL
  trace %TRACE);

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

# Protected - for use by subclasses only
# Register reserved words
sub reserve {
    my $this = shift;
    foreach (@_) {
        $this->{reserved}->{$_} = 1;
    }
}

=begin TML

---++ ObjectMethod encode_utf8($s) -> $e

Some drivers require you to encode to UTF8 (DBD::SQLite), and some don't
(DBD::Pg). The default is to encode.

=cut

sub encode_utf8 {
    my $this = shift;
    return Encode::encode_utf8( $_[0] );
}

=begin TML

---++ ObjectMethod decode_utf8($e) -> $s

Some drivers require you to decode from UTF8 (DBD::SQLite), and some don't
(DBD::Pg)

=cut

sub decode_utf8 {
    my $this = shift;
    return Encode::decode_utf8( $_[0] );
}

=begin TML

---++ ObjectMethod startup($dbh)
Execute any SQL commands required to start the DB in ANSI mode.
Subclasses must call superclass.

=cut

sub startup {
    my ( $this, $dbh ) = @_;
    ASSERT($dbh) if DEBUG;
    $this->{dbh} = $dbh;
}

=begin TML

---++ ObjectMethod table_exists(table_name [, table_name]*) -> boolean
Determine if a table exists. All tables named in parameters
must exist.

=cut

sub table_exists {
    my $this = shift;
    my $tables = join( ',', map { $this->safe_data($_) } @_ );
    ASSERT( $this->{dbh} ) if DEBUG;

    # MySQL, Postgresql, MS SQL Server
    my $sql = <<SQL;
SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES
 WHERE TABLE_NAME IN ($tables)
SQL

    trace($sql) if $TRACE{sql};
    my $rows = $this->{dbh}->selectall_arrayref( $this->encode_utf8($sql) );
    return scalar(@$rows) == scalar(@_);
}

=begin TML

---++ ObjectMethod get_columns(table_name) -> %cols
Get a map of column names to type for the given table

=cut

sub get_columns {
    my ( $this, $table ) = @_;
    ASSERT( $this->{dbh} ) if DEBUG;

    # MySQL, Postgresql, MS SQL Server
    my $sql = <<SQL;
SELECT COLUMN_NAME, DATA_TYPE FROM INFORMATION_SCHEMA.COLUMNS 
 WHERE TABLE_NAME = '$table'
SQL

    trace($sql) if $TRACE{sql};
    my $s = $this->{dbh}->selectall_arrayref( $this->encode_utf8($sql) );
    return { map { ( $_->[0] => $_->[1] ) } @$s };
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

---++ ObjectMethod safe_id($id) -> $safeid

Make sure the ID is safe to use in this dialect of SQL.
Unsafe IDs should be quoted using the dialect's identifier
quoting rule.

The default is to double-quote all identifiers.
 If $this->no_high_bit()
is true, then all high-bit characters are encoded as XN where N is the
decimal codepoint of the character.

Note that this is only used on idenitifers derived from Foswiki data
e.g. table and column names.

=cut

sub safe_id {
    my ( $this, $id ) = @_;

    # blunt instrument - protect against all non-word chars
    $id =~ s/([^\w])/'X'.ord("\1")/ges;
    return "\"$id\"";
}

=begin TML

---++ ObjectMethod safe_data($data) -> $safedata
Make sure the data is safe to use in this dialect of SQL.
Unsafe data should be quoted using the dialect's data
quoting rule. The default is to single-quote all identifiers.

=cut

sub safe_data {
    my ( $this, $data ) = @_;

    # double up single quotes
    $data =~ s/'/''/g;
    return "'$data'";
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
