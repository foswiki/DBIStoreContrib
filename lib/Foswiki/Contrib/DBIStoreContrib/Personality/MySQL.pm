# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::Personality::MySQL;

# DBIStoreContrib personality module for MySQL

use strict;
use warnings;
use Foswiki::Contrib::DBIStoreContrib qw(trace %TRACE);

use Foswiki::Contrib::DBIStoreContrib::Personality ();
our @ISA = ('Foswiki::Contrib::DBIStoreContrib::Personality');

# Use the database version this has been tested with
our $VERSION = '5.7';

sub new {
    my ( $class, $dbistore ) = @_;
    my $this = $class->SUPER::new($dbistore);
    $this->reserve(
        qw/
          ACCESSIBLE ANALYZE ASENSITIVE BEFORE BIGINT BINARY BLOB BOTH CALL
          CHANGE CHAR CHARACTER CONDITION CONTINUE CONVERT CURRENT_USER CURSOR
          DATABASE DATABASES DAY_HOUR DAY_MICROSECOND DAY_MINUTE DAY_SECOND DEC
          DECIMAL DECLARE DELAYED DESCRIBE DETERMINISTIC DISTINCTROW DIV DOUBLE
          DUAL EACH ELSEIF ENCLOSED ESCAPED EXIT EXPLAIN FALSE FETCH FLOAT
          FLOAT4 FLOAT8 FORCE FULLTEXT GET GRANT HIGH_PRIORITY HOUR_MICROSECOND
          HOUR_MINUTE HOUR_SECOND IF IGNORE INFILE INOUT INSENSITIVE INT INT1
          INT2 INT3 INT4 INT8 INTEGER INTERVAL IO_AFTER_GTIDS IO_BEFORE_GTIDS
          ITERATE KEYS KILL LEADING LEAVE LIMIT LINEAR LINES LOAD LOCALTIME
          LOCALTIMESTAMP LOCK LONG LONGBLOB LONGTEXT LOOP LOW_PRIORITY
          MASTER_BIND MASTER_SSL_VERIFY_SERVER_CERT MATCH MAXVALUE MEDIUMBLOB
          MEDIUMINT MEDIUMTEXT MIDDLEINT MINUTE_MICROSECOND MINUTE_SECOND MOD
          MODIFIES NATURAL NO_WRITE_TO_BINLOG NONBLOCKING NUMERIC OPTIMIZE
          OPTION OPTIONALLY OUT OUTFILE PARTITION PRECISION PROCEDURE PURGE
          RANGE READ READ_WRITE READS REAL REGEXP RELEASE RENAME REPEAT REPLACE
          REQUIRE RESIGNAL RETURN REVOKE RLIKE SCHEMA SCHEMAS SECOND_MICROSECOND
          SENSITIVE SEPARATOR SHOW SIGNAL SMALLINT SPATIAL SPECIFIC SQL
          SQL_BIG_RESULT SQL_CALC_FOUND_ROWS SQL_SMALL_RESULT SQLEXCEPTION
          SQLSTATE SQLWARNING SSL STARTING STRAIGHT_JOIN TERMINATED TINYBLOB
          TINYINT TINYTEXT TRAILING TRIGGER TRUE UNDO UNLOCK UNSIGNED USAGE USE
          USING UTC_DATE UTC_TIME UTC_TIMESTAMP VARBINARY VARCHAR VARCHARACTER
          VARYING WHILE WRITE XOR YEAR_MONTH ZEROFILL/
    );
    return $this;
}

sub startup {
    my ( $this, $dbh ) = @_;
    $this->SUPER::startup($dbh);

    # MySQL has to be kicked in the ANSIs
    $this->sql("SET sql_mode='ANSI'");
    $this->sql('SELECT @sql_mode');

    # set to UTF8
    $this->sql('SET NAMES utf8');
}

# MySQL driver wants everything handed to it on a plate
sub to_db {
    return Encode::encode_utf8( $_[1] );
}

# MySQL driver wants you to deal with its crap
sub from_db {
    return Encode::decode_utf8( $_[1] );
}

sub regexp {
    my ( $this, $sexpr, $pat ) = @_;

    unless ( $pat =~ s/^'(.*)'$/$1/s ) {

        # Somewhat risky....
        return "$sexpr REGEXP ($pat)";
    }

    # MySQL uses POSIX regular expressions.

    # POSIX has no support for (?i: etc
    $pat =~ s/^\(\?[a-z]+:(.*)\)$/$1/;                # remove (?:i)
                                                      # Nor hex character codes
    $pat =~ s/\\x([0-9a-f]{2})/_char("0x$1")/gei;
    $pat =~ s/\\x\{([0-9a-f]+)\}/_char("0x$1")/gei;

    # Nor \d, \D
    $pat =~ s/(^|(?<=[^\\]))\\d/[0-9]/g;
    $pat =~ s/(^|(?<=[^\\]))\\D/[^0-9]/g;

    # Nor \b, \B
    $pat =~ s/\\\\[bB](.*?)\\\\[bB]/\[\[:<:\]\]$1\[\[:>:\]\]/g;
    $pat =~ s/\\\\[bB]($|\|)/\[\[:>:\]\]$1/g;
    $pat =~ s/(^|\|)\\\\[bB]/$1\[\[:<:\]\]/g;

    # Nor \s, \S, \w, \W
    $pat =~ s/(^|(?<=[^\\]))\\s/[ \011\012\015]/g;
    $pat =~ s/(^|(?<=[^\\]))\\S/[^ \011\012\015]/g;
    $pat =~ s/(^|(?<=[^\\]))\\w/[a-zA-Z0-9_]/g;
    $pat =~ s/(^|(?<=[^\\]))\\W/[^a-zA-Z0-9_]/g;

    # Convert X? to (X|)
    #$pat =~ s/(?<=[^\\])(\(.*\)|\[.*?\]|\\.|.)\?/($1|)/g;    # ?
    $pat =~ s/([\+\*])\?/$1/g;
    $pat =~ s/\?://g;

    # Handle special characters
    $pat =~ s/(?<=[^\\])\\n/\n/g;             # will this work?
    $pat =~ s/(?<=[^\\])\\r/\r/g;
    $pat =~ s/(?<=[^\\])\\t/\t/g;
    $pat =~ s/(?<=[^\\])\\b//g;               # not supported
    $pat =~ s/(?<=[^\\])\{\d+(,\d*)?\}//g;    # not supported
                                              # Escape '
                                              #$pat =~ s/\\/\\\\/g;

    return "$sexpr REGEXP ('$pat')";
}

sub cast_to_numeric {
    my ( $this, $d ) = @_;
    return "CAST(($d) AS DECIMAL)";
}

sub cast_to_text {
    my ( $this, $d ) = @_;
    return "CAST(($d) AS CHAR)";
}

sub length {
    return "char_length($_[1])";
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
