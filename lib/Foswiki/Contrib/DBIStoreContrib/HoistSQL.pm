# See bottom of file for copyright and license details

=begin TML

---+ package Foswiki::Contrib::DBIStoreContrib::HoistSQL

Static functions to extract SQL expressions from queries. The SQL is
used to pre-filter topics cached in an SQL DB for more efficient
query matching.

=cut

package Foswiki::Contrib::DBIStoreContrib::HoistSQL;

use strict;
use Assert;

use Foswiki::Contrib::DBIStoreContrib qw(%TRACE
  trace personality $TABLE_PREFIX
  NAME NUMBER STRING UNKNOWN BOOLEAN SELECTOR VALUE TABLE PSEUDO_BOOL);
use Foswiki::Infix::Node                             ();
use Foswiki::Query::Node                             ();
use Foswiki::Query::Parser                           ();
use Foswiki::Store::QueryAlgorithms::DBIStoreContrib ();
use Foswiki::Func                                    ();

# A Foswiki query parser
our $parser;

our $table_name_RE = qr/^\w+$/;

# Pseudo-constants, from the Personality
our $TRUE;
our $TRUE_TYPE;

BEGIN {

    # Foswiki 1.1 doesn't have makeConstant; monkey-patch it
    unless ( defined &Foswiki::Infix::Node::makeConstant ) {
        *Foswiki::Infix::Node::makeConstant = sub {
            my ( $this, $type, $val ) = @_;
            $this->{op}     = $type;
            $this->{params} = [$val];
          }
    }
}

# Coverage; add _COVER(__LINE__) where you want a visit recorded.
use constant COVER => 0;
our %covered;

BEGIN {
    if (COVER) {
        open( F, '<', __FILE__ );
        local $/ = "\n";
        my $lno = 0;
        while ( my $l = <F> ) {
            $lno++;
            if ( $l =~ /_COVER\(__LINE__\)/ ) {
                $covered{$lno} = 0;
            }
        }
        close(F);
    }
}

sub _COVER {
    $covered{ $_[0] }++;
}

END {
    if (COVER) {
        open( F, '>', "coverage" );
        print F join( "\n", map { "$_ $covered{$_}" } sort keys %covered )
          . "\n";
        close(F);
    }
}

# Frequently used SQL constructs

# Generate SQL SELECT statement. %opts are:
#     comment: comment, usually caller's __LINE__, only included if tracing
#     options permit it
#     select: what to select e.g. *
#     FROM: where to select from
#     WHERE: condition
sub _SELECT {
    my (%opts) = @_;
    my $info = '';
    if ( $TRACE{sql} && defined $opts{comment} ) {
        $info = personality->make_comment( $opts{comment} );
    }
    my $pick = $opts{select};
    if ( ref($pick) ) {
        $pick = join( ',', @$pick );
    }
    my $sql = "SELECT$info $pick";
    foreach my $clause (qw(FROM WHERE)) {
        next unless defined $opts{$clause};
        my $val = $opts{$clause};
        $val = [$val] unless ( ref($val) );
        $sql .= " $clause "
          . join( ',', map { $_ =~ /^SELECT/ ? "($_)" : $_ } @$val );
    }
    return $sql;
}

# Generate AS statement
# _AS(thing => alias)
sub _AS {
    my %args = @_;
    my @terms;
    while ( my ( $what, $alias ) = each %args ) {
        if ( defined $alias ) {
            $what = "($what)"
              if $what !~ /^(\w|`)+$/
              && $what !~ /^#T?<[^>]*>$/
              && $what !~ /^(["']).*\1$/
              && $what !~ /^\([^()]*\)$/;
            push( @terms, "$what AS $alias" );
        }
        else {
            push( @terms, $what );
        }
    }
    return join( ',', @terms );
}

# Generate UNION statement
sub _UNION {
    my ( $a, $b ) = @_;
    return "$a UNION $b";
}

sub _abort {
    throw Error::Simple(
        join( ' ', 'SQL generator:', map { ref($_) ? recreate($_) : $_ } @_ ) );
}

# Mapping operators to SQL. Functions accept the arguments and their
# inferred types.
sub _simple_uop {

    # Simple unary operator
    my ( $opn, $type, $arg, $atype ) = @_;
    $arg = _cast( $arg, $atype, $type );
    return ( "$opn($arg)", $type );
}

sub _boolean_uop {
    my ( $opn, $arg, $atype ) = @_;
    return _simple_uop( $opn, BOOLEAN, $arg, $atype );
}

my %uop_map = (
    not => sub { _boolean_uop( 'NOT', @_ ) },
    lc     => sub { _simple_uop( 'LOWER', STRING, @_ ) },
    uc     => sub { _simple_uop( 'UPPER', STRING, @_ ) },
    length => sub {
        my ( $arg, $atype ) = @_;
        if ( $atype != STRING && $atype != UNKNOWN ) {
            $arg = _cast( $arg, $atype, STRING );
        }
        return ( personality->length($arg), NUMBER );
    },
    d2n => sub {
        my ( $arg, $atype ) = @_;
        if ( $atype != NUMBER ) {
            $arg = personality->d2n($arg);
        }
        return ( $arg, NUMBER );
    },
    int => sub { },                                  # handled in rewrite
    '-' => sub { _simple_uop( '-', NUMBER, @_ ) },
    '+' => sub { _simple_uop( '+', NUMBER, @_ ) },
);

# A bop returning a number
sub _numeric_bop {
    my ( $opn, $lhs, $lhs_type, $rhs, $rhs_type ) = @_;
    $lhs = _cast( $lhs, $lhs_type, NUMBER );
    $rhs = _cast( $rhs, $rhs_type, NUMBER );
    return ( "($lhs)$opn($rhs)", NUMBER );
}

# A bop returning a number or a string
sub _flexi_bop {
    my ( $opn, $lhs, $lhs_type, $rhs, $rhs_type ) = @_;
    my $ot = NUMBER;
    if ( $lhs_type == STRING || $rhs_type == STRING ) {
        $ot = STRING;
    }
    $lhs = _cast( $lhs, $lhs_type, $ot );
    $rhs = _cast( $rhs, $rhs_type, $ot );
    return ( "($lhs)$opn($rhs)", $ot );
}

# A bop returning a boolean
sub _boolean_bop {
    my ( $opn, $lhs, $lhs_type, $rhs, $rhs_type ) = @_;
    if ( $lhs_type == NUMBER || $rhs_type == NUMBER ) {
        $lhs = _cast( $lhs, $lhs_type, NUMBER );
        $rhs = _cast( $rhs, $rhs_type, NUMBER );
    }
    elsif ( $lhs_type == STRING || $rhs_type == STRING ) {
        $lhs = _cast( $lhs, $lhs_type, STRING );
        $rhs = _cast( $rhs, $rhs_type, STRING );
    }
    else {
        $lhs = _cast( $lhs, $lhs_type, BOOLEAN );
        $rhs = _cast( $rhs, $rhs_type, BOOLEAN );
    }
    return ( "($lhs)$opn($rhs)", BOOLEAN );
}

my %bop_map = (
    and  => sub { _boolean_bop( 'AND', @_ ) },
    or   => sub { _boolean_bop( 'OR',  @_ ) },
    ','  => sub { _abort("Unsupported ',' operator"); },
    'in' => sub { _abort("Unsupported 'in' operator"); },
    '-'  => sub { _flexi_bop( '-',     @_ ) },
    '+'  => sub { _flexi_bop( '+',     @_ ) },
    '*'  => sub { _numeric_bop( '*',   @_ ) },
    '/'  => sub { _numeric_bop( '/',   @_ ) },
    '='  => sub {
        my ( $lhs, $lhs_type, $rhs, $rhs_type ) = @_;

        # Special case
        if ( $lhs eq 'NULL' ) {
            if ( $rhs eq 'NULL' ) {
                return ( $TRUE, $TRUE_TYPE );
            }

            # Need EXISTS condition
            return ( "($rhs) IS NULL", BOOLEAN );
        }
        elsif ( $rhs eq 'NULL' ) {

            # Need EXISTS condition
            return ( "($lhs) IS NULL", BOOLEAN );
        }
        return _boolean_bop( '=', @_ );
    },
    '!=' => sub { _boolean_bop( '!=', @_ ) },
    '<'  => sub { _boolean_bop( '<',  @_ ) },
    '>'  => sub { _boolean_bop( '>',  @_ ) },
    '<=' => sub { _boolean_bop( '<=', @_ ) },
    '>=' => sub { _boolean_bop( '>=', @_ ) },
    '~'  => sub {
        my ( $lhs, $lhst, $rhs, $rhst ) = @_;
        my $expr = personality->wildcard( $lhs, $rhs );
        return ( $expr, BOOLEAN );
    },
    '=~' => sub {
        my ( $sexpr, $lhst, $pat, $rhst ) = @_;
        my $expr = personality->regexp( $sexpr, $pat );
        return ( $expr, BOOLEAN );
    },

);

# Generate a unique alias for a table or column
my $temp_id = 0;

sub _alias {
    my $line = shift;
    my $tid = 't' . ( $temp_id++ );
    $tid .= "_$line" if $TRACE{sql};
    return $tid;
}

=begin TML

---++ ObjectMethod hoist($query) -> $sql_statement

The main method in this module generates SQL from a query. The return value
is a valid, stand-alone SQL query. This is a complete mapping of a
Foswiki query to SQL.

Will die with a message if there is a diagnosable problem.

=cut

sub hoist {
    my ($query) = @_;

    unless ( defined $TRUE ) {
        $TRUE      = personality->{true_value};
        $TRUE_TYPE = personality->{true_type};
    }

    my $original;

    if ( $TRACE{hoist} ) {
        $original = recreate($query);
        trace( 'Hoisting ', $original );
    }

    # Simplify the parse tree, work out type information.
    $query = _clarifyFWQuery( $query, UNKNOWN, ' ' );

    if ( $TRACE{hoist} ) {
        trace( ' Clarified ', $original, ' as ', recreate($query) );
    }

    my %h = _hoist( $query, '#<topic>' );

    my $alias = _alias(__LINE__);    # SQL server requires this!
    if ( $h{is_table_name} ) {
        $h{sql} =
          "#T<topic>.#<tid> IN (SELECT #<tid> FROM ($h{sql}) AS $alias)";
    }
    elsif ( $h{is_select} ) {

        # It's a table; test if the selector is a true value
        my $where = '';
        if ( $h{sel} ) {
            $where = " WHERE " . personality->is_true( $h{type}, $h{sel} );
        }
        my $a2 = _alias(__LINE__);    # SQL server requires this!
             # This rather clumsy construction is required because SQL server
             # can't use an aliased column in the WHERE condition of the same
             # SELECT.
        $h{sql} =
"#T<topic>.#<tid> IN (SELECT #<tid> FROM (SELECT * FROM ($h{sql}) AS $a2 $where) AS $alias)";
    }
    else {
        $h{sql} = personality->is_true( $h{type}, $h{sql} );
    }
    trace( ' Built SQL ', $h{sql} ) if $TRACE{hoist};

    return $h{sql};
}

# The function that does the actual work of hoisting a clarified
# FWQ expression.
# Params: ($node, $in_table)
# $node - the FWQ node being processed
# $in_table - the table in which lookup is being performed. A simple ID.
#
# Return: %result with keys:
# sql => the generated SQL
# type => type of the subexpression
# is_table_name => true if the statement yields a table (even if not a SELECT)
# selector => optional selector indicating the single column name chosen
# in the subquery
# ignore_tid => set true if the tids in the result come from a subquery
# over an unrelated topic. Such tids are not propagated up through
# boolean operations.
#
# Any sub-expression generates a query that yields a table. That table
# has an associated column (or columns). So,
#
# TOPICINFO yields (SELECT * FROM TOPICINFO) if it's used raw
#
# TOPICINFO.blah yields (SELECT blah FROM TOPICINFO)
#
# fields[name="blah"] yields (SELECT * FROM FIELD WHERE name="blah")
#
# 'Topic'/fields[name='blah'].value yields (SELECT value FROM (SELECT * FROM topic,(SELECT * FROM FIELD WHERE name="blah") AS t1 WHERE topic.tid=t1.tid AND topic.name="Topic")
#
# tname/sexpr yields
# (SELECT * FROM topic,sexpr AS t1 WHERE topic.tid=t1.tid AND topic.name=tname
#                                                             [ $lhs_where   ]
sub _hoist {
    my ( $node, $in_table ) = @_;

    # The default context table is 'topic'. As soon as we go into a
    # SELECT, the context table may change. In a /, the context table
    # is still 'topic'
    trace( "HOIST ", $node ) if $TRACE{hoist};

    my $op    = $node->{op}->{name}         if ref( $node->{op} );
    my $arity = scalar @{ $node->{params} } if ref( $node->{op} );

    my %result;
    $result{type} = UNKNOWN;

    if ( !ref( $node->{op} ) ) {
        if ( $node->{op} == STRING ) {

            # Convert to an escaped SQL string
            my $s = $node->{params}[0];
            $s =~ s/\\/\\\\/g;

            # Escape single quote by doubling it (SQL standard)
            $s =~ s/'/''/gs;
            $result{sql}  = "'$s'";
            $result{type} = STRING;
        }
        elsif ( $node->{op} == NAME ) {

            # A simple name
            my $name = $node->{params}[0];
            if ( $name =~ /^META:(\w+)$/ ) {

                # Name of a table
                $result{sql}           = "#T<$1>";
                $result{is_table_name} = 1;
                $result{type}          = STRING;
            }
            elsif ( $name eq 'undefined' ) {
                $result{sql}  = 'NULL';
                $result{type} = UNKNOWN;
            }
            else {

                # Name of a field
                $result{sql} = ( $in_table ? "$in_table." : '' ) . "#<$name>";
                $result{type} = STRING;
            }
        }
        else {
            $result{sql}  = $node->{params}[0];
            $result{type} = $node->{op};
        }
    }
    elsif ( $op eq '[' ) {
        my %lhs = _hoist( $node->{params}[0], $in_table );

        my $from_alias;
        my $tid_constraint = '';
        if ( $lhs{is_table_name} || $lhs{is_select} ) {

            $from_alias = _alias(__LINE__);
            $lhs{sql} = _AS( $lhs{sql} => $from_alias );

            if ( $lhs{is_table_name} && $in_table ) {

#-MySQL                $tid_constraint = " AND $from_alias.#<tid>=$in_table.#<tid>";
            }
        }
        else {
            _abort( "Expected a table on the LHS of '[':", $node );
        }

        my %where = _hoist( $node->{params}[1], $from_alias );

        if ( $where{is_select} ) {
            $where{sql} = "EXISTS($where{sql})";
        }
        elsif ( $where{is_table_name} ) {

            # Hum. TABLE[TABLE_NAME]
            _abort( "Cannot use a table name here:",
                $node, $node->{params}[1] );
        }
        else {
            $where{sql} = personality->is_true( $where{type}, $where{sql} );
        }

        my $where = "$where{sql}$tid_constraint";

        $result{sql} = _SELECT(
            select  => '*',
            FROM    => $lhs{sql},
            WHERE   => $where,
            comment => __LINE__
        );
        $result{is_select}  = 1;
        $result{has_where}  = 1 if length($where);
        $result{type}       = STRING;
        $result{ignore_tid} = 1 if $lhs{ignore_tid};

        # No . here, so no selector
    }
    elsif ( $op eq '.' ) {
        my %lhs = _hoist( $node->{params}[0], $in_table );

        # SMELL: ought to be able to support an expression generating
        # a selector name on the RHS. But that's just too hard in SQL.
        my $rhs = $node->{params}[1];
        if ( ref( $rhs->{op} ) || $rhs->{op} != NAME ) {
            _abort( "Expected a selector name on the RHS of '.':", $node );
        }
        $result{sel} = $rhs->{params}[0];

        my $alias   = _alias(__LINE__);
        my @selects = ("$alias.#<tid>");
        if ( $lhs{is_select} ) {
            push( @selects, $result{sel} );
        }
        elsif ( $lhs{is_table_name} ) {
            push( @selects, "$alias.#<$result{sel}>" );
        }
        else {
            _abort( "Expected a table on the LHS of '.':", $node );
        }
        $result{sql} = _SELECT(
            select  => \@selects,
            FROM    => _AS( $lhs{sql} => $alias ),
            comment => __LINE__
        );
        $result{is_select}  = 1;
        $result{type}       = STRING;
        $result{ignore_tid} = 1 if $lhs{ignore_tid};

    }
    elsif ( $op eq '/' ) {

        # A lookup in another topic

        my $topic_alias = _alias(__LINE__);

        # Expect a condition that yields a topic name on the LHS
        my $lhs = $node->{params}[0];
        my %lhs = _hoist( $node->{params}[0], undef );
        my $lhs_where;
        my @selects;
        my $wtn =
          personality->strcat( "$topic_alias.#<web>", "'.'",
            "$topic_alias.#<name>" );
        if ( $lhs{is_select} ) {
            my $tnames = _alias(__LINE__);
            push( @selects, _AS( $lhs{sql} => $tnames ) );
            my $tname_sel = $tnames;
            $tname_sel = "$tnames.#<$lhs{sel}>" if $lhs{sel};
            $lhs_where =
              "($topic_alias.#<name>=$tname_sel OR ($wtn)=$tname_sel)";
        }
        elsif ( $lhs{is_table_name} ) {

            # Table name with no select. Useless.
            _abort( "Table name cannot be used here:", $node, $lhs );
        }
        else {

            # Not a selector or simple table name, must be a simple
            # expression yielding a selector
            $lhs_where = "($lhs{sql}) IN ($topic_alias.#<name>,$wtn)";
        }

        # Expand the RHS *without* a constraint on the topic table
        my %rhs = _hoist( $node->{params}[1], undef );
        unless ( $rhs{is_select} || $rhs{is_table_name} ) {

            # We *could* handle this without an error, but it would
            # be pretty meaningless e.g. 'Topic'/1
            _abort( "Expected a table expression on the RHS of '/':", $node );
        }
        $result{sel} = $rhs{sel};

        my $sexpr_alias = _alias(__LINE__);

        my $tid_constraint = "$sexpr_alias.#<tid> IN ("
          . _SELECT(
            select  => '#<tid>',
            FROM    => _AS( '#T<topic>' => $topic_alias ),
            WHERE   => $lhs_where,
            comment => __LINE__
          ) . ")";

        push( @selects, _AS( $rhs{sql} => $sexpr_alias ) );
        $result{sql} = _SELECT(

            # select all columns (which will include tid)
            select  => "$sexpr_alias.*",
            FROM    => \@selects,
            WHERE   => $tid_constraint,
            comment => __LINE__
        );

        $result{is_select}  = 1;
        $result{has_where}  = 1;
        $result{type}       = $rhs{type};
        $result{ignore_tid} = 1;
    }
    elsif ( $arity == 2 && defined $bop_map{$op} ) {
        my $lhs = $node->{params}[0];
        my %lhs = _hoist( $lhs, $in_table );

        my $rhs = $node->{params}[1];
        my %rhs = _hoist( $rhs, $in_table );

        my $opfn = $bop_map{$op};

        if (   ( $lhs{is_select} || $lhs{is_table_name} )
            && ( $rhs{is_select} || $rhs{is_table_name} ) )
        {

            # TABLE - TABLE

            my $lhs_alias = _alias(__LINE__);
            my $rhs_alias = _alias(__LINE__);

            $result{sel} = _alias(__LINE__);
            if ( $op eq 'or' ) {

                # Special case for OR, because the OR operator
                # doesn't work the way the other operators do when
                # it's used on two tables. Not sure why, it ought
                # to work AFAICT from RTFM, but it doesn't.
                my $union_alias = _alias(__LINE__);
                my ( $lhs_sql, $rhs_sql );

                if ( $lhs{ignore_tid} ) {

                    # Don't propagate tids from the LHS
                    $lhs_sql = _SELECT(
                        select  => _AS( '#<tid>', $lhs_alias ),
                        FROM    => '#T<topic>',
                        WHERE   => 'EXISTS(' . $lhs{sql} . ')',
                        comment => __LINE__
                    );
                }
                else {
                    $lhs_sql = _SELECT(
                        select  => '#<tid>',
                        FROM    => _AS( $lhs{sql}, $lhs_alias ),
                        comment => __LINE__
                    );
                }

                if ( $rhs{ignore_tid} ) {

                    # Don't propagate tids from the RHS
                    $rhs_sql = _SELECT(
                        select  => _AS( '#<tid>', $rhs_alias ),
                        FROM    => '#T<topic>',
                        WHERE   => 'EXISTS(' . $rhs{sql} . ')',
                        comment => __LINE__
                    );
                }
                else {
                    $rhs_sql = _SELECT(
                        select  => '#<tid>',
                        FROM    => _AS( $rhs{sql}, $rhs_alias ),
                        comment => __LINE__
                    );
                }

                my $union_sql = _UNION( $lhs_sql, $rhs_sql );

                $result{sql} = _SELECT(
                    select =>
                      [ 'DISTINCT ' . _AS( $TRUE => $result{sel} ), '#<tid>' ],
                    FROM    => _AS( $union_sql, $union_alias ),
                    comment => __LINE__
                );
                $result{is_select}  = 1;
                $result{type}       = $TRUE_TYPE;
                $result{ignore_tid} = 0;
            }
            else {
                # All other non-OR table-table operators
                if ( defined $rhs{sel} ) {
                    $lhs{sel} = $rhs{sel} unless defined $lhs{sel};
                }
                elsif ( defined $lhs{sel} ) {
                    $rhs{sel} = $lhs{sel};
                }
                else {
                    _abort(
"Cannot '$op' two tables without at least one selector:",
                        $node
                    );
                }
                my $l_sel = "$lhs_alias.#<$lhs{sel}>";
                my $r_sel = "$rhs_alias.#<$rhs{sel}>";

                my ( $expr, $optype ) = &$opfn(
                    $l_sel => $lhs{type},
                    $r_sel => $rhs{type}
                );
                my $where = "($lhs_alias.#<tid>=$rhs_alias.#<tid>)";
                if ( $optype == BOOLEAN ) {

                    $where .= " AND ($expr)";
                    $expr   = $TRUE;
                    $optype = $TRUE_TYPE;
                }

                my $ret_tid   = "$lhs_alias.#<tid>";
                my $tid_table = '';
                if ( $rhs{ignore_tid} || $lhs{ignore_tid} ) {
                    $ret_tid   = '#T<topic>.#<tid>';
                    $tid_table = '#T<topic>,';
                }
                $result{sql} = _SELECT(
                    select =>
                      [ 'DISTINCT ' . _AS( $expr => $result{sel} ), $ret_tid ],
                    FROM => $tid_table
                      . _AS(
                        $lhs{sql} => $lhs_alias,
                        $rhs{sql} => $rhs_alias
                      ),
                    WHERE   => $where,
                    comment => __LINE__
                );
                $result{is_select} = 1;
                $result{has_where} = length($where);
                $result{type}      = $optype;
            }
        }
        elsif ( $lhs{is_select} || $lhs{is_table_name} ) {

            # TABLE - CONSTANT
            my $operate = sub {
                my $sel = shift;
                return &$opfn(
                    $sel      => $lhs{type},
                    $rhs{sql} => $rhs{type}
                );
            };
            _genSingleTableSELECT( \%lhs, $operate, \%result,
                __LINE__ . " $op" );
        }
        elsif ( $rhs{is_select} ) {

            # CONSTANT - TABLE
            my $operate = sub {
                my $sel = shift;
                return &$opfn(
                    $lhs{sql} => $lhs{type},
                    $sel      => $rhs{type}
                );
            };
            _genSingleTableSELECT( \%rhs, $operate, \%result,
                __LINE__ . " $op" );
        }
        else {

            # CONSTANT - CONSTANT
            ( $result{sql}, $result{type} ) = &$opfn(
                $lhs{sql} => $lhs{type},
                $rhs{sql} => $rhs{type}
            );
        }
    }
    elsif ( $arity == 1 && defined $uop_map{$op} ) {
        my $opfn = $uop_map{$op};
        my %kid = _hoist( $node->{params}[0], $in_table );
        if ( $kid{is_select} || $kid{is_table_name} ) {
            my $operate = sub {
                my $sel = shift;
                return &$opfn( $sel => UNKNOWN );
            };
            _genSingleTableSELECT( \%kid, $operate, \%result,
                __LINE__ . " $op" );

        }
        else {
            ( $result{sql}, $result{type} ) = &$opfn( $kid{sql}, $kid{type} );
        }
        $result{ignore_tid} = 1 if $kid{ignore_tid};
    }
    else {
        _abort( "Don't know how to hoist '$op':", $node );
    }
    trace( "Hoisted ", \%result ) if $TRACE{hoist};
    return %result;
}

sub _genSingleTableSELECT {
    my ( $table, $operate, $result, $monitor ) = @_;

    my $alias = _alias(__LINE__);
    my $sel   = $alias;
    $sel = "$alias.#<$table->{sel}>" if $table->{sel};

    $result->{sel}        = _alias(__LINE__);
    $result->{ignore_tid} = 0;
    my ( $expr, $optype ) = &$operate($sel);

    my $where;
    if ( $optype == BOOLEAN ) {
        $where  = $expr;
        $expr   = $TRUE;
        $optype = $TRUE_TYPE;
    }

    my $ret_tid = "$alias.#<tid>";
    my @froms = ( _AS( $table->{sql} => $alias ) );
    if ( $table->{ignore_tid} ) {

        # ignore tid coming from the subexpression
        $ret_tid = '#T<topic>.#<tid>';
        unshift( @froms, '#T<topic>' );
    }

    $result->{sql} = _SELECT(
        select  => [ _AS( $expr => $result->{sel} ), $ret_tid ],
        FROM    => \@froms,
        WHERE   => $where,
        comment => $monitor
    );
    $result->{is_select} = 1;
    $result->{has_where} = length($where);
    $result->{type}      = $optype;
}

# Generate a cast statement, if necessary.
# from a child node type.
# $arg - the SQL being cast
# $type - the current type of the $arg (may be UNKNOWN)
# $tgt_type - the target type of the cast (may be UNKNOWN)
sub _cast {
    my ( $arg, $type, $tgt_type ) = @_;
    return $arg if $tgt_type == UNKNOWN || $type == $tgt_type;
    if ( $tgt_type == BOOLEAN ) {
        $arg = personality->is_true( $type, $arg );
    }
    elsif ( $tgt_type == NUMBER ) {
        $arg = personality->cast_to_numeric($arg);
    }
    elsif ( $type != UNKNOWN ) {
        $arg = personality->cast_to_text($arg);
    }
    return $arg;
}

# _clarifyFWQuery( $node, $context, $indent ) -> $node
# Rewrite a Foswiki query parse tree rooted at $node to prepare it for
# SQL hoisting. This mainly consists of resolving context dependencies
# in the expression, and rewriting shorthand expressions to their full form.
# $context is one of:
#    * UNKNOWN - the node being processed is in the context of the topic table
#    * VALUE - context of a WHERE
#    * TABLE - context of a table expression e.g. LHS of a [
# $indent is used when debugging for clean printing.
sub _clarifyFWQuery {
    my ( $node, $context, $indent ) = @_;

    my $before;
    $before = recreate($node) if $TRACE{hoist};
    my $lineNo = __LINE__;

    my $op    = $node->{op};
    my $arity = scalar @{ $node->{params} };

    # We only handle at most binary ops, so must unflatten n-ary ops, such
    # as and and or. Do this by taking params 0 and 1 and creating a clone
    # of this node
    if ( ref($op) ) {
        while ( $arity > 2 ) {
            my $p0 = shift @{ $node->{params} };
            $node->{params}[0] =
              Foswiki::Infix::Node->newNode( $node->{op}, $p0,
                $node->{params}[0] );
            $arity--;
        }
    }

    if ( !ref($op) ) {
        if ( $op == NAME ) {
            my $name = $node->{params}[0];

            # Map to long form if available e.g. 'form' to 'META:FORM'
            my $tname = $Foswiki::Query::Node::aliases{$name} || $name;

            if ( $context == UNKNOWN ) {

                # the node being processed is in the context of the topic table

                # A name floating around loose in an expression is
                # implicitly a column in the topic table. Rewrite it as a
                # context-free expression.
                $parser ||= new Foswiki::Query::Parser();
                if ( $name =~ /^(name|web|text|raw)$/ ) {

                    $node = _clarifyFWQuery( $parser->parse("META:topic.$name"),
                        $context, "$indent " );
                    $lineNo = __LINE__;
                }
                elsif ( $name ne 'undefined' ) {

                    # A table on it's own in the root doesn't make
                    # a lot of sense. Deal with it anyway.
                    if ( $tname =~ /^META:\w+$/ ) {
                        $node->{params}[0] = $tname;
                        $node->{is_table}  = 1;
                        $lineNo            = __LINE__;
                    }
                    else {
                        # name refers to a field in the table. Rewrite it as
                        # a context-free expression.
                        $node = _clarifyFWQuery(
                            $parser->parse("META:FIELD[name='$name'].value"),
                            $context, "$indent " );
                        $lineNo = __LINE__;
                    }
                }
            }
            elsif ( $context == TABLE ) {

                # LHS of a [

                if ( $tname =~ /^META:\w+$/ ) {
                    $node->{params}[0] = $tname;
                    $node->{is_table}  = 1;
                    $lineNo            = __LINE__;
                }
                else {

                    # An unknown name where a table is expected?
                    # It may be a form name?
                    $lineNo = __LINE__;
                }
            }
            else {    # $context = VALUE
                      # in a WHERE

                if ( $tname =~ /^META:\w+$/ ) {

                    # Rewrite using the META: name and tag as a table
                    $node->{params}[0] = $tname;
                    $node->{is_table}  = 1;
                    $lineNo            = __LINE__;
                }
                else {

                    # Name used as a selector
                    $node->{is_selector} = 1;
                    $lineNo = __LINE__;
                }
            }
        }
        else {

            # STRING or NUMBER. Foswiki doesn't distinguish them :-(
            $lineNo = __LINE__;
        }
    }
    elsif ( $op->{name} eq '(' ) {

        # Can simply collapse this
        $node = _clarifyFWQuery( $node->{params}[0], $context, "$indent " );
        $lineNo = __LINE__;
    }
    elsif ( $op->{name} eq 'int' ) {

        # Can simply collapse this
        $node = _clarifyFWQuery( $node->{params}[0], $context, "$indent " );
        $lineNo = __LINE__;
    }
    elsif ( $op->{name} eq '.' ) {

        # The legacy of the . operator means it really requires context
        # information to determine how it should be parsed. We don't have
        # all that context here, so we have to do the best we can with what
        # we have, and rewrite it as a []
        my $lhs = _clarifyFWQuery( $node->{params}[0], TABLE, "$indent " );
        my $rhs = _clarifyFWQuery( $node->{params}[1], VALUE, "$indent " );

        unless ( $lhs->{is_table} ) {
            trace( __LINE__ . " lhs may not be a table." . recreate($lhs) )
              if $TRACE{hoist};
        }

        # RHS must be a key.
        _abort( "Illegal RHS of '.':", $rhs ) unless $rhs->{is_selector};

        if ( !$lhs->{is_table} && !ref( $lhs->{op} ) ) {

            # The LHS must be a form name. Since we only support
            # one form per topic, we can rewrite this as a field select,
            # and add a constraint on the topic using the META:FORM table.
            # Constraint is "META:FORM.name='$lhs->{params}[0]'"
            $parser ||= new Foswiki::Query::Parser();

            # Must rewrite to infer types
            $node = _clarifyFWQuery(
                $parser->parse("META:FIELD[name='$rhs->{params}[0]'].value"),
                $context, "$indent " );
            $lineNo = __LINE__;
        }
        else {

            # The result of the subquery might be a table or a single
            # value, but either way we have to treat it as a table.
            $node->{is_table} = 1;
            $lineNo = __LINE__;
        }
    }
    elsif ( $op->{name} eq '[' ) {

        my $lhs = _clarifyFWQuery( $node->{params}[0], TABLE, "$indent " );
        my $rhs = _clarifyFWQuery( $node->{params}[1], VALUE, "$indent " );

        $node->{is_table} = 1;
    }
    else {
        for ( my $i = 0 ; $i < $arity ; $i++ ) {
            my $nn =
              _clarifyFWQuery( $node->{params}[$i], $context, "$indent " );
            $node->{params}[$i] = $nn;
        }
    }

    trace( $indent, $lineNo, ': Clarified FWQ ',
        $before, ' as ', recreate($node) )
      if $TRACE{hoist};
    return $node;
}

# Simple SQL formatter for the type of expression generated by this module
sub _format_SQL {
    my ($sql) = @_;

    # Assumes balanced brackets - won't work if \( or \) present
    my @ss = ();

    # Replace escaped quotes
    $sql =~ s/('')/push(@ss,$1); "![$#ss]!"/ges;

    # Replace quoted strings
    $sql =~ s/('([^'])*')/push(@ss,$1); "![$#ss]!"/ges;

    # Replace bracketed subexpressions
    my $n = 0;
    while ( $sql =~ s/\(([^\(\)]*?)\)/<$n:$1:$n>/s ) {
        $n++;
    }

    # Find and format the first region
    $sql = _format_region( $sql, '' );

    # Break remaining long lines on AND and OR
    my @lines = split( /\n/, $sql );
    my @nlines = ();
    foreach my $line (@lines) {
        my $ind = '';
        if ( $line =~ /^(\s+)/ ) {
            $ind = $1;
        }
        $ind = " $ind";
        while ( $line =~ /[^\n]{80}/ ) {
            last unless ( $line =~ s/(.{5}.*?\S +)(AND|OR|ORDER)/$ind$2/s );
            push( @nlines, $1 );
        }
        push( @nlines, $line ) if $line =~ /\S/;
    }
    $sql = join( "\n", @nlines );

    # Replace strings
    while ( $sql =~ s/!\[(\d+)\]!/$ss[$1]/gs ) {
    }
    return $sql;
}

sub _format_region {
    my ( $sql, $indent ) = @_;
    if ( $sql =~ /^(.*?)<(\d+):(.*):\2>(.*)$/s ) {
        my ( $before, $subexpr, $after ) = ( $1, $3, $4 );
        $before =~ s/(?<=\S) +(FROM|WHERE|UNION|SELECT)\b/\n$indent$1/gs;
        my $abrack    = '';
        my $subindent = $indent;
        if ( $subexpr =~ /^SELECT/ ) {
            $before    .= "\n$indent";
            $subindent .= " ";
            $abrack = "\n$indent";
        }
        $sql =
            "$before("
          . _format_region( $subexpr, $subindent )
          . "$abrack)"
          . _format_region( $after, $indent );
    }
    else {
        $sql =~ s/(?<=\S) +(FROM|WHERE|UNION|SELECT)\b/\n$indent$1/gs;
    }
    return $sql;
}

=begin TML

---++ StaticMethod recreate( $node ) -> $string

Unparse a Foswiki query expression from a parse tree. Should be
part of the Foswiki::Query::Node class, but isn't :-(

=cut

sub recreate {
    my ( $node, $pprec ) = @_;
    $pprec ||= 0;
    my $s;

    if ( ref( $node->{op} ) ) {
        my @oa;
        my $arity = scalar @{ $node->{params} };
        for ( my $i = 0 ; $i < $arity ; $i++ ) {
            my $nprec = $node->{op}->{prec};
            $nprec++ if $i > 0;
            $nprec = 0 if $node->{op}->{close};
            push( @oa, recreate( $node->{params}[$i], $nprec ) );
        }
        my $nop = $node->{op}->{name};
        if ( scalar(@oa) == 1 ) {
            if ( $node->{op}->{close} ) {
                $s = "$node->{op}->{name}$oa[0]$node->{op}->{close}";
            }
            else {
                $nop = "$nop " if $nop =~ /\w$/ && $oa[0] =~ /^\w/;
                $s = "$nop$oa[0]";
            }
        }
        elsif ( scalar(@oa) == 2 ) {
            if ( $node->{op}->{close} ) {
                $s = "$oa[0]$node->{op}->{name}$oa[1]$node->{op}->{close}";
            }
            else {
                $nop = " $nop" if ( $nop =~ /^\w/ && $oa[0] =~ /[\w)]$/ );
                $nop = "$nop " if ( $nop =~ /\w$/ && $oa[1] =~ /^\w/ );
                $s = "$oa[0]$nop$oa[1]";
            }
        }
        else {
            $s = join( " $nop ", @oa );
        }
        $s = "($s)" if ( $node->{op}->{prec} < $pprec );
    }
    elsif ( $node->{op} == STRING ) {
        $s = $node->{params}[0];
        $s =~ s/\\/\\\\/g;
        $s = "'$s'";
    }
    else {
        $s = $node->{params}[0];
    }
    return $s;
}

1;
__DATA__

Module of Foswiki - The Free and Open Source Wiki, http://foswiki.org/, http://Foswiki.org/

Copyright (C) 2014-2017 Foswiki Contributors. All Rights Reserved.
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

Author: Crawford Currie http://c-dot.co.uk
