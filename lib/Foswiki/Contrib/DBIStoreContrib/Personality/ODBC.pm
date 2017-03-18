# See bottom of file for license and copyright information
package Foswiki::Contrib::DBIStoreContrib::Personality::ODBC;

# Personality module for generic ODBC

use strict;
use warnings;

use Foswiki::Contrib::DBIStoreContrib qw(NAME NUMBER STRING UNKNOWN
  BOOLEAN SELECTOR VALUE TABLE PSEUDO_BOOL);
use Foswiki::Contrib::DBIStoreContrib::Personality ();
our @ISA = ('Foswiki::Contrib::DBIStoreContrib::Personality');

# Use the database version this has been tested with
our $VERSION = 'ODBC';

sub new {
    my ( $class, $dbistore ) = @_;
    my $this = $class->SUPER::new($dbistore);
    $this->reserve(
        qw/
          ANY AUTHORIZATION BACKUP BEGIN BREAK BROWSE BULK CHECKPOINT CLOSE
          CLUSTERED COALESCE COMMIT COMPUTE CONTAINS CONTAINSTABLE CONTINUE
          CONVERT CURRENT CURRENT_USER CURSOR DATABASE DBCC DEALLOCATE DECLARE
          DENY DISK DISTRIBUTED DOUBLE DUMP END ERRLVL ESCAPE EXCEPT EXEC
          EXECUTE EXIT EXTERNAL FETCH FILE FILLFACTOR FREETEXT FREETEXTTABLE
          FULL FUNCTION GOTO GRANT HOLDLOCK IDENTITY IDENTITY_INSERT IDENTITYCOL
          IF INTERSECT KILL LINENO LOAD MERGE NATIONAL NOCHECK NONCLUSTERED
          NULLIF OF OFF OFFSETS OPEN OPENDATASOURCE OPENQUERY OPENROWSET OPENXML
          OPTION OVER PERCENT PIVOT PLAN PRECISION PRINT PROC PROCEDURE PUBLIC
          RAISERROR READ READTEXT RECONFIGURE REPLICATION RESTORE RETURN REVERT
          REVOKE ROLLBACK ROWCOUNT ROWGUIDCOL RULE SAVE SCHEMA SECURITYAUDIT
          SEMANTICKEYPHRASETABLE SEMANTICSIMILARITYDETAILSTABLE
          SEMANTICSIMILARITYTABLE SESSION_USER SETUSER SHUTDOWN SOME STATISTICS
          SYSTEM_USER TABLESAMPLE TEXTSIZE TOP TRAN TRANSACTION TRIGGER TRUNCATE
          TRY_CONVERT TSEQUAL UNPIVOT UPDATETEXT USE USER VARYING VIEW WAITFOR
          WHILE WITHIN GROUP WRITETEXT/
    );

    # Override the default type in the schema
    $Foswiki::cfg{Extensions}{DBIStoreContrib}{Schema}{_DEFAULT}{type} =
      'VARCHAR(MAX)';
    $this->{true_value}      = '(1=1)';    #'CAST(1 AS BIT)';
    $this->{true_type}       = BOOLEAN;    #PSEUDO_BOOL;
    $this->{requires_COMMIT} = 0;

    return $this;
}

sub startup {
    my ( $this, $dbh ) = @_;
    $this->SUPER::startup($dbh);
}

sub is_true {
    my ( $this, $type, $sql ) = @_;

    $type = STRING if $type == UNKNOWN;

    return $this->SUPER::is_true( $type, $sql );
}

sub length {
    return "LEN($_[1])";
}

sub strcat {
    my $this = shift;
    return join( '+', @_ );
}

sub d2n {
    my ( $this, $arg ) = @_;

    # Convert date string to number
    # 22088800 = number of seconds from 1900-01-01 to start of epoch
    # 86400 = number of seconds in a day
    return "(CAST(CONVERT(datetime, $arg) AS FLOAT) * 86400 - 22088800)";
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

