#!/usr/bin/env perl
use strict;

# for net IO
use LWP::Simple qw($ua get);
$ua->timeout(300);

#
# Silly lolcode.com interpreter. Released under the BSD license 5/30/2007
# Copyright (c) 2007, Joe Drago <joedrago@gmail.com>
# All rights reserved.
#
# ***
# Now a MAD ADVANCED LOLCode interpreter, released under the BSD license,
# since that's what Joe did.
# 
# YALI
# . Yet Another Lolcode Interpreter
# . YALI: Another Lolcode Interpreter
# Updates 2008-03-19 by Kyle Woodward <1@618034.com>
#
# http://1.618034.com/yali-yet-another-lolcode-interpreter/index.php
#

my $variable_match = qr/^[a-z_][a-z_0-9]*$/io;
my $number_match = qr/^\-?([0-9]+(\.[0-9]+)?|[0-9]*\.[0-9]+)(e-?\d{1,3})?$/io;
my $string_match = qr/^"([^"]|:")*"$/io;

$::RD_TRACE = undef;
$::RD_HINT = undef;

use Parse::RecDescent;
use Data::Dumper;

my $grammar = q {
# ---------------------------------------------------------------------
# Core stuff

lolfile      : lol_program
             | { ::showerrors(); } # Allows me to handle errors my way

lol_program  : <skip: qr/[ \t]*/> allowed_line(s) eofile 
                                                             {$item[2]}
             | <error>

allowed_line : statement
             | ignored_line

ignored_line : comment
             | blankline

comment      : commentsigil anything(?) "\n" {0} # Ignored by interpreter

statement    : /s*/ IM_IN_YR "\n"
                                                             {$item[2]}
             | /s*/ IZ_ORLY "\n"
                                                             {$item[2]}
             | /s*/ command "\n"
                                                             {$item[2]}
blankline    : /\s+/ {0}

# ---------------------------------------------------------------------
# The big list of all commands

command      : HAI
             | DIAF
             | DO_NOT_WANTZ
             | FOUND_YR
             | GIMMEH
             | GTFO
             | HOW_DUZ_I
             | I_HAS_A
             | IM_IN_YR
             | IZ
             | KTHXBYE
             | LOL
             | OBTW
             | UP
             | VISIBLE

comparator   : BIGGER_THAN
             | LIEK
             | SMALLER_THAN
             | SORTA

reserved_keywords : 'BIGR'
                  | 'IN'
                  | 'SMALR'
                  | 'DEN'
                  | 'IZ'
                  | 'GTFO'
                  | 'BOOMZ'
                  | 'OVERZ'
                  | 'TIEMZ'
                  | 'NERF'
                  | 'UP'
                  | 'DIAF'
                  | 'LIEK' # etc. etc.

# ---------------------------------------------------------------------
# Command syntax

IZ_ORLY      : 'IZ' comparator 'O' "RLY?" "\n" 
                 ignored_line(s?)
                'YA RLY' "\n"
                allowed_line(s) 
                'NO WAI' "\n"
                allowed_line(s) 
                'KTHX'
{ {IZEXEC=>$item[2], YESEXEC=>$item[9], NOEXEC=>$item[12], CMD=>$item[0]} }
        
             | 'IZ' comparator 'O' "RLY?" "\n" 
                 ignored_line(s?)
                'YA RLY' "\n"
                allowed_line(s) 
                'KTHX'
    { {IZEXEC=>$item[2], YESEXEC=>$item[9], NOEXEC=>0, CMD=>$item[0]} }

             | 'IZ' comparator 'O' "RLY?" "\n" 
                 ignored_line(s?)
                'NO WAI' "\n"
                allowed_line(s) 
                'KTHX'
    { {IZEXEC=>$item[2], YESEXEC=>0, NOEXEC=>$item[9], CMD=>$item[0]} }

IZ             : 'IZ' comparator <skip: '\s*'> '?' <skip: '\s*'> command
               {
                {
                  IZEXEC => $item[2],
                  YESEXEC => $item[6],
                  CMD => $item[0]
                }
               }

IM_IN_YR     : 'IM' 'IN' 'YR' variable_name "\n"
                allowed_line(s)
                'KTHX'
                                { {LOOPEXEC=>$item[6], CMD=>$item[0]} }

HAI          : 'HAI' anything
                    { {LINE=>$thisline, CMD=>$item[0], STR=>$item[2]} }
             | 'HAI' 
                                   { {LINE=>$thisline, CMD=>$item[0]} }

KTHXBYE      : 'KTHXBYE'
             {
              {
                LINE => $thisline,
                CMD => $item[0]
              }
             }

DIAF         : 'DIAF' function
             {
              {
                CMD => $item[0],
                FN => $item[2],
                LINE => $thisline
              }
             }
             | 'DIAF'
             {
              {
                CMD => $item[0],
                FN => undef,
                LINE => $thisline
              }
             }

GTFO         : 'GTFO'
                                   { {LINE=>$thisline, CMD=>$item[0]} }

KTHX         : 'KTHX'
                                   { {LINE=>$thisline, CMD=>$item[0]} }

I_HAS_A      : 'I' 'HAS' 'A' variable 'ITZ' function
             {
              {
                VAL => $item[6],
                LINE => $thisline,
                CMD => $item[0],
                VAR => $item[4]
              }
             }
             | 'I' 'HAS' 'A' variable
             {
              {
                LINE => $thisline,
                CMD => $item[0],
                VAR=>$item[4]
              }
             }

LOL          : 'LOL' variable_name 'R' function
             {
              {
                CMD => $item[0],
                VAR => $item[2],
                VAL => $item[4],
                LINE => $thisline
              }
             }

VISIBLE      : 'VISIBLE' identifier '!'
             {
              {
                NOBREAK => 1,
                LINE => $thisline,
                CMD => $item[0],
                DEST => 'STDOUT',
                VAR => $item[2]
              }
             }
             | 'VISIBLE' identifier
             {
              {
                LINE => $thisline,
                DEST => 'STDOUT',
                CMD => $item[0],
                VAR=>$item[2]
              }
             }
             | 'VISIBLE' variable_name identifier
             {
              {
                CMD => $item[0],
                VAR => $item[3],
                DEST => $item[2],
                LINE => $thisline
              }
             }

#
# Just do a command rewrite
#
UP           : <skip: qr//> 'UP' /[ \t]*/ variable_name '!!' number
             {
              {
                CMD => 'LOL',
                VAR => $item[4],
                VAL => {
                  FN => {
                    LHS => $item[4],
                    OPR => 'UP',
                    RHS => {
                      ATOM => [
                        $item[6]
                      ]
                    }
                  },
                  PRMS => undef
                },
                LINE => $thisline
              }
             }
             | <skip: qr//> 'UP' /[ \t]*/ variable_name '!!'
             {
              {
                CMD => 'LOL',
                VAR => $item[4],
                VAL => {
                  FN => {
                    LHS => $item[4],
                    OPR => 'UP',
                    RHS => {
                      ATOM => [
                        1
                      ]
                    }
                  },
                  PRMS => undef
                },
                LINE => $thisline
              }
             }

BIGGER_THAN  : formula 'BIGR' 'DEN' formula
             {
              {
                RHS => $item[4],
                LINE => $thisline,
                CMD => $item[0],
                LHS => $item[1]
              }
             }

SMALLER_THAN : formula 'SMALR' 'DEN' formula
             {
              {
                RHS => $item[4],
                LINE => $thisline,
                CMD => $item[0],
                LHS => $item[1]
              }
             }

SORTA        : variable 'SORTA' string_list
             {
              {
                RHS => $item[3],
                LINE => $thisline,
                CMD => $item[0],
                LHS => $item[1]
              }
             }

LIEK         : formula 'LIEK' formula
             {
              {
                LHS => $item[1],
                RHS => $item[3],
                CMD => $item[0],
                LINE => $thisline
              }
             }
             | formula 'NOT' 'LIEK' formula
             {
              {
                LHS => $item[1],
                RHS => $item[4],
                CMD => $item[0],
                NOT => 1,
                LINE => $thisline
              }
             }

#
# KW 2008-02-22 addendum
#
GIMMEH       : 'GIMMEH' inputtype variable_name 'OUTTA' variable
             {
              {
                CMD => $item[0],
                TYPE => $item[2],
                VAR => $item[3],
                SRC => $item[5]
              }
             }
             | 'GIMMEH' inputtype variable_name
             {
              {
                CMD => $item[0],
                TYPE => $item[2],
                VAR => $item[3],
                SRC => {
                  ATOM => ['STDIN']
                }
              }
             }

DO_NOT_WANTZ : 'DO' 'NOT' 'WANTZ' variable
             {
              {
                CMD => $item[0],
                VAR => $item[4]
              }
             }

HOW_DUZ_I    : 'HOW' 'DUZ' 'I' variable_name 'YR' param_list "\n"
               allowed_line(s?)
               'IF' 'U' 'SAY' 'SO'
             {
              {
                CMD => 'HOW_DUZ_I',
                NAME => $item[4],
                PRMS => $item[6],
                EXEC => $item[8],
                LINE => $thisline
              }
             }
             | 'HOW' 'DUZ' 'I' variable_name "\n"
               allowed_line(s?)
               'IF' 'U' 'SAY' 'SO'
             {
              {
                CMD => 'HOW_DUZ_I',
                NAME => $item[4],
                PRMS => undef,
                EXEC => $item[6],
                LINE => $thisline
              }
             }

FOUND_YR     : 'FOUND' 'YR' function
             {
              {
                CMD => 'FOUND_YR',
                LINE => $thisline,
                VAL => $item[3]
              }
             }

OBTW         : 'OBTW' anything "\n"
               more_comments
             {0}

more_comments: 'TLDR'
             {0}
             | anything "\n"
               more_comments
             {0}

# ---------------------------------------------------------------------
# Basic symbols

#identifier_list : identifier 'N' identifier_list
#                {
#                 [
#                   $item[1],
#                   @{$item[3]}
#                 ]
#               }
#               | identifier 
#                {
#                 [
#                   $item[1]
#                 ]
#               }

expression_list : variable 'AN' 'YR' expression_list
                {
                  [
                    $item[1],
                    @{$item[4]}
                  ]
                }
                | variable 'MKAY'
                {
                  [
                    $item[1]
                  ]
                }

param_list   : variable_name 'AN' 'YR' param_list
             {
              {
                ATOM => $item[1],
                MORE => $item[4]
              }
             }
             | variable_name
             {
              {
                ATOM => $item[1]
              }
             }

identifier   : formula

string       : /^"([^"]|:")*"/

string_list  : variable_name 'N' string_list
             {
              [
                $item[1],
                @{$item[3]}
              ]
             }
             | string 'N' string_list
             {
              [
                [$item[1]],
                @{$item[3]}
              ]
             }
             | variable_name
             {
              [
                $item[1]
              ]
             }
             | string
             {
              [
                [
                  $item[1]
                ]
              ]
             }

number       : /^\-?([0-9]+(\.[0-9]+)?|[0-9]*\.[0-9]+)(e-?\d{1,3})?/o
             {
              $item[1]
             }

variable     : variable_name 'N' string_list
             {
              {
                STR => 1,
                ATOM => [
                  $item[1],
                  @{$item[3]}
                ]
              }
             }
             | variable_name
             {
              {
                ATOM => $item[1]
              }
             }
             | number
             {
              {
                ATOM => [$item[1]]
              }
             }
             | string_list
             {
              {
                STR => 1,
                ATOM => $item[1]
              }
             }

#
# Note that we could structure this just as well
# as variable_name IN MAH /[a-z][a-z0-9]*|[0-9]+/i,
# but we'd have to change all of the array-parsing logic
#
# ...
#
# I no longer recall the purpose of that comment.
#
variable_name: /^([a-z_][a-z_0-9]*|[0-9]+)/io 'IN' 'MAH' variable_name
             {
              [
                $item[1],
                @{$item[4]}
              ]
             }
             | /^[a-z_][a-z_0-9]*/io
             {
              [
                $item[1]
              ]
             }

formula      : variable_name operator formula
             {
              {
                LHS => $item[1],
                OPR => $item[2],
                RHS => $item[3]
              }
             }
             | '(' formula ')'
             {
              $item[2]
             }
             | variable
             {$item[1]}

function     : 'MEBBE' function 'MEBBE' function
             {
              {
                FN => undef,
                KW => $item[1],
                PRMS => [$item[2],$item[4]]
              }
             }
             | 'ALL' variable
             {
              {
                FN => undef,
                KW => $item[1],
                PRMS => [$item[2]]
              }
             }
             | 'TYEP' variable
             {
              {
                FN => undef,
                KW => $item[1],
                PRMS => [$item[2]]
              }
             }
             | 'SORTA' variable
             {
              {
                FN => undef,
                KW => $item[1],
                PRMS => [$item[2]]
              }
             }
             | function_name 'YR' expression_list
             {
              {
                FN => $item[1],
                KW => undef,
                PRMS => $item[3]
              }
             }
             | function_name 'MKAY'
             {
              {
                FN => $item[1],
                KW => undef,
                PRMS => []
              }
             }
             | identifier
             {
              {
                FN => $item[1],
                KW => undef,
                PRMS => undef
              }
             }

function_name: /^[a-z_][a-z_0-9]*/io
             {
              {
                ATOM => $item[1]
              }
             }

operator     : 'UP'
             | 'NERF'
             | 'OVARZ'
             | 'TIEMZ'
             | 'BOOMZ'

anything     : /[^\n]*/

commentsigil : /^\^\^/
             | 'BTW'

eofile       : /^\Z/

inputtype    : 'LINEZ'
             | 'LINE'
             | 'WURD'
             | 'LETTAR'
             | 'NUMBR'

# ---------------------------------------------------------------------
};





#
# Actually begin the script
#
my $usage = <<EOU;
Usage: perl lol.pl -s [...] [-h]
  -s IZ WERDZ N FILE
  -h IZ M0AR
EOU

my $source_file = '';

#
# Load arguments, according to usage (above)
#
while ( 1 )
{
  my $arg = lc shift;
  last if ( length $arg < 1 );

  if ( $arg eq '-s' )
  {
    $source_file = shift;
  }
  elsif ( $arg eq '-h' )
  {
    print $usage;
    exit;
  }
  else
  {
    print 'DO NOT WANTZ "'.$arg.'"; GIMMEH -h 4 M0AR.'."\n\n";
    exit;
  }
}

if ( !-e $source_file )
{
  print 'INVISIBLE "'.$source_file.'"; GTFO.'."\n\n";
  exit;
}

#
# Attempt to parse the script passed
#
my $parser = Parse::RecDescent->new($grammar) or die "Bad grammar!\n";

my $text = "";
{
    local $/ = undef;
    open HELLO, "< $source_file" or die "cant read input\n";
    $text = <HELLO>;
    close(HELLO);
}

my $tree = $parser->lolfile($text);
if(!defined($tree))
{
    { 
        foreach (@{$parser->{errors}})
        {
            print "Line $_->[1]:$_->[0]\n";
        }
        $parser->{errors} = undef;

        undef;
    }

    exit;
}

my $fn_table = {};
for ( 0..$#{@$tree} )
{
  if ( !$tree->[$_] )
  {
    next;
  }

  if ( $tree->[$_]->{CMD} eq 'HOW_DUZ_I' )
  {
    my $fn_definition = $tree->[$_];
    my $fn_name = $fn_definition->{NAME}->[0];

    $fn_table->{$fn_name}->{VARS} = [];
    $fn_table->{$fn_name}->{EXEC} = $fn_definition->{EXEC};
    $fn_table->{$fn_name}->{PRMS} = [];

    if ( $fn_definition->{PRMS} )
    {
      my $params = $fn_definition->{PRMS};
      my $param_index = 0;

      while ( 1 )
      {
        $fn_table->{$fn_name}->{PRMS}->[$param_index] = $params->{ATOM}->[0];
        #$fn_table->{$fn_name}->{VARS}->[$depth]->{ATOM} = undef;

        if ( !defined $params->{MORE} )
        {
          last;
        }

        $params = $params->{MORE};
        ++$param_index;
      }
    }
  }
}

my $regex_matches = [];

#print "MSG: Grammar parsed.\n\n";
$fn_table->{_}->{VARS} = [];
exec_lol($tree,'_');

exit;






# ---------------------------------------------------------------------
# Variable table and other interpreter state

my $breaking; # = 0;

# ---------------------------------------------------------------------





#
# value_from_identifier
# -----
# Takes an identifier and a line # and attempts to parse to a value.
#
# $identifier should be:
# {
#   ATOM => [name,ind1,ind2,...]
# }
#
sub value_from_identifier
{
  my($identifier,$namespace,$depth,$line) = @_;

  #
  # For whatever reason, we've received a non-nested argument. Pass to evaluate_formula
  # to re-wrap it for us.
  #
  if ( !defined $identifier->{ATOM} )
  {
    return evaluate_formula($identifier,$namespace,$depth,$line);
  }

  if ( defined $identifier->{STR} )
  {
    return string_value($identifier->{ATOM},$namespace,$depth,$line);
  }

  $identifier = $identifier->{ATOM};
  my $i_len = scalar @$identifier;
  if ( $identifier->[$i_len-1] =~ $number_match )
  {
    return value($identifier->[$i_len-1]);
  }
  elsif ( $identifier->[$i_len-1] =~ /^"(([^"]|:")*)"$/ )
  {
    return value($1);
  }

  if ( !defined $fn_table->{$namespace}->{VARS}->[$depth]->{$identifier->[$i_len-1]} )
  {
    die sprintf('Error: attempting to read undeclared variable "%s" in "%s" at line %d. Aborting.'."\n",$identifier->[$i_len-1],$namespace,$line);
  }

  my $array = $fn_table->{$namespace}->{VARS}->[$depth]->{$identifier->[$i_len-1]}->[0];
  for ( reverse 0..$i_len - 2 )
  {
    my $index = evaluate_formula(atomic($identifier->[$_]),$namespace,$depth,$line);

    if ( $index->{VALUE} !~ /^\d+$/ or !defined $array->{ARRAY} or !defined $array->{ARRAY}->[$index->{VALUE}] )
    {
      #
      # We no longer want to throw an error here. Just create the thing and keep going.
      #
      #die sprintf('Error: malformed array index (evaluates to "%s") at line %d. Aborting.'."\n",$index->{VALUE},$line);
      $array->{ARRAY}->[$index->{VALUE}] = array([]);
    }

    $array = $array->{ARRAY}->[$index->{VALUE}];
  }

  return $array;
}
#
# /value_from_identifier
#





#
# string_value
# -----
# Concatenates a string and returns a value.
#
sub string_value
{
  my ($string,$namespace,$depth,$line) = @_;

  my $string_value = '';
  for ( 0..scalar @$string - 1 )
  {
    my $item_value = value_from_identifier({ATOM=>$string->[$_]},$namespace,$depth,$line);

    if ( defined $item_value->{ARRAY} )
    {
      $item_value = $item_value->{ARRAY};
    }
    else
    {
      $item_value = $item_value->{VALUE}
    }

    if ( $item_value =~ /^"(.*)"$/ )
    {
      $item_value = $1;
    }
    $string_value .= $item_value;
  }

  $string_value =~ s/\\\\/\\/g;
  $string_value =~ s/\\n/\n/g;
  $string_value =~ s/\\r/\r/g;
  $string_value =~ s/\\t/\t/g;
  return value($string_value);
}






#
# call_keyword
# -----
# Takes a keyword and produces the appropriate output from the internal functions.
#
sub call_keyword
{
  my ($call,$namespace,$depth,$line) = @_;

  if ( $call->{KW} eq 'MEBBE' )
  {
    my $lower_bound;
    my $upper_bound;

    if ( defined $fn_table->{$call->{PRMS}->[0]->{FN}->{ATOM}}->{VARS} )
    {
      $lower_bound = evaluate_function($call->{PRMS}->[0],$namespace,$depth,$line);
    }
    else
    {
      $lower_bound = evaluate_formula($call->{PRMS}->[0]->{FN},$namespace,$depth,$line);
    }

    if ( defined $fn_table->{$call->{PRMS}->[1]->{FN}->{ATOM}}->{VARS} )
    {
      $upper_bound = evaluate_function($call->{PRMS}->[1],$namespace,$depth,$line);
    }
    else
    {
      $upper_bound = evaluate_formula($call->{PRMS}->[1]->{FN},$namespace,$depth,$line);
    }

    if ( !defined $lower_bound->{VALUE} )
    {
      die 'Error: malformed initial value in MEBBE call on line '.$line.'. Aborting.'."\n";
    }

    if ( !defined $upper_bound->{VALUE} )
    {
      die 'Error: malformed secondary value in MEBBE call on line '.$line.'. Aborting.'."\n";
    }

    my $value = rand;
    $value *= $upper_bound->{VALUE} - $lower_bound->{VALUE} + 1;
    $value += $lower_bound->{VALUE};
    return value(int $value);
  }

  if ( $call->{KW} eq 'ALL' )
  {
    my $array = value_from_identifier($call->{PRMS}->[0],$namespace,$depth,$line);

    if ( !defined $array->{ARRAY} )
    {
      if ( !defined $array->{VALUE} )
      {
        return value(0);
      }

      die 'Error: ALL call on non-array object at line '.$line.'. Aborting.'."\n";
    }

    return value(scalar @{$array->{ARRAY}});
  }

  if ( $call->{KW} eq 'TYEP' )
  {
    my $value = value_from_identifier($call->{PRMS}->[0],$namespace,$depth,$line);

    if ( defined $value->{ARRAY} )
    {
      return value('ARRAY');
    }

    return value('VALUE');
  }

  if ( $call->{KW} eq 'SORTA' )
  {
    my $value = value_from_identifier($call->{PRMS}->[0],$namespace,$depth,$line);

    return value($regex_matches->[$value->{VALUE}]);
  }

  die 'Error: no handler for keyword "'.$call->{KW}.'" on line '.$line.'. Aborting.'."\n";
}
#
# /call_keyword
#





#
# Just some helper functions to make dealing with all this easier.
#
sub atomic
{
  my ($v) = @_;

  return {
    ATOM => [ $v ]
  };
}





sub value
{
  hash(shift,undef);
}





sub hash
{
  {
    VALUE => shift,
    ARRAY => shift
  };
}





sub array
{
  hash(undef,shift);
}
#
# /helpers
#





#
# set_variable
# -----
#
# New set_variable function.
#
# Takes a variable array [name,index1,index2,...] and a value, and sets appropriately.
#
# $value looks like:
# $->{VALUE} = x | undef
# $->{ARRAY} = undef | x
#
sub set_variable
{
  my ($variable,$value,$namespace,$depth,$line,$create) = @_;

  my $v_len = scalar @$variable;
  #
  # Can we find the variable? If not, and we're not supposed to create it, bug out.
  #
  if ( !defined $fn_table->{$namespace}->{VARS}->[$depth]->{$variable->[$v_len-1]} )
  {
    if ( !$create )
    {
      die sprintf('Error: attempt to set undefined variable "%s" in "%s" on line %d. Aborting.'."\n",$variable->[$v_len-1],$namespace,$line);
    }

    $fn_table->{$namespace}->{VARS}->[$depth]->{$variable->[$v_len-1]}->[0] = {};
  }

  #
  # If the variable has no subsequent indices, just set it to the passed value.
  #
  if ( scalar @$variable == 1 )
  {
    if ( defined $value )
    {
      $fn_table->{$namespace}->{VARS}->[$depth]->{$variable->[0]}->[0] = $value;
    }
    else
    {
      $fn_table->{$namespace}->{VARS}->[$depth]->{$variable->[0]}->[0] = array;
    }
    
    return;
  }

  #
  # Fast-forward to the proper address
  #
  my $array = $fn_table->{$namespace}->{VARS}->[$depth]->{$variable->[$v_len-1]}->[0];
  for ( reverse 0..$v_len - 2 )
  { 
    my $index = evaluate_formula(atomic($variable->[$_]),$namespace,$depth,$line);
    if ( !defined $index->{VALUE} or $index->{VALUE} !~ /^\d+$/ )
    {
      die sprintf('Error: bad array index at position %d of "%s" in "%s" on line %d. Aborting.'."\n",$_,$variable->[$v_len-1],$namespace,$line);
    }
    
    if ( !defined $array->{ARRAY} )
    {
      $array->{VALUE} = undef;
      $array->{ARRAY} = [];
    }
    
    if ( !defined $array->{ARRAY}->[$index->{VALUE}] )
    {
      $array->{ARRAY}->[$index->{VALUE}] = {};
    }

    $array = $array->{ARRAY}->[$index->{VALUE}];
  }

  if ( $value->{ARRAY} )
  {
    $array->{VALUE} = undef;
    $array->{ARRAY} = $value->{ARRAY};
  }
  elsif ( $value )
  {
    $array->{VALUE} = $value->{VALUE};
    $array->{ARRAY} = undef;
  }
}
#
# /set_variable
#





#
# evaluate_formula
# -----
# Simple two-part formula evaluation.
#
sub evaluate_formula
{
  my ($formula,$namespace,$depth,$line) = @_;

  if ( defined $formula->{ATOM} ) {
    return value_from_identifier($formula,$namespace,$depth,$line);
  }

  my $val_1 = value_from_identifier({ATOM=>$formula->{LHS}},$namespace,$depth,$line);
  my $val_2 = evaluate_formula($formula->{RHS},$namespace,$depth,$line);

  if ( !defined $val_1->{VALUE} )
  {
    #
    # Ignore the error for now.
    #
    $val_1 = value(0);
    #die sprintf('Error: array reference in formula at line %d. Aborting.'."\n",$line);
  }

  $val_1 = $val_1->{VALUE};
  $val_2 = $val_2->{VALUE};
  if ( $val_1 !~ /^\-?([0-9]+(\.[0-9]+)?|[0-9]*(\.[0-9]+)?)(e-?\d{1,3})?$/o )
  {
    die sprintf('Error: invalid number (LHS) %d at line %d. Aborting.'."\n",$val_1,$line);
  }
  if ( $val_2 !~ /^\-?([0-9]+(\.[0-9]+)?|[0-9]*(\.[0-9]+)?)(e-?\d{1,3})?$/o )
  {
    die sprintf('Error: invalid number (RHS) %d at line %d. Aborting.'."\n",$val_2,$line);
  }

  my $operator = $formula->{OPR};
  my $return = undef;

  if ( $operator eq 'UP' )
  {
    $return = $val_1 + $val_2;
  }
  elsif ( $operator eq 'NERF' )
  {
    $return = $val_1 - $val_2;
  }
  elsif ( $operator eq 'OVARZ' )
  {
    if ( $val_2 == 0 )
    {
      die 'Divide by zero error in "'.$formula.'". Exiting program.'."\n\n";
    }
      
    $return = $val_1 / $val_2;
  }
  elsif ( $operator eq 'TIEMZ' )
  {
    $return = $val_1 * $val_2;
  }
  elsif ( $operator eq 'BOOMZ' )
  {
    $return = $val_1 ** $val_2;
  }
  else
  {
    die 'Malformed formula "'.$formula.'". Exiting program.'."\n\n";
  }

  return value($return);
}
#
# /evaluate_formula
#





#
# evaluate_function
# -----
# Gets the value from a function identifier.
#
sub evaluate_function
{
  my ($function,$namespace,$depth,$line) = @_;

  my $function_name = $function->{FN}->{ATOM};
  if ( !defined $fn_table->{$function_name} )
  {
    die 'Call to unrecognized function "'.$function_name.'" at line '.$line.'. Aborting.';
  }

  my $parameters = $function->{PRMS};
  if ( scalar @$parameters > scalar @{$fn_table->{$function_name}->{PRMS}} )
  {
    die 'Too many parameters passed to "'.$function_name.'" on line '.$line.'. Aborting.';
  }
  elsif ( scalar @$parameters > scalar @{$fn_table->{$function_name}->{PRMS}} )
  {
    die 'Not enough parameters passed to "'.$function_name.'" on line '.$line.'. Aborting.';
  }

  for ( 0..scalar @{$fn_table->{$function_name}->{PRMS}} - 1 )
  {
    my $parameter_name = $fn_table->{$function_name}->{PRMS}->[$_];
    my $value = value_from_identifier($parameters->[$_],$namespace,$depth,$line);
    
    if ( $value->{ARRAY} )
    {
      $fn_table->{$function_name}->{VARS}->[$depth+1]->{$parameter_name}->[0] = $value;
    }
    else
    {
      $fn_table->{$function_name}->{VARS}->[$depth+1]->{$parameter_name}->[0] = {
        VALUE => $value->{VALUE},
        ARRAY => undef
      };
    }
  }

  my $val = exec_lol($fn_table->{$function_name}->{EXEC},$function_name,$depth+1);
  return $val;
}
#
# /evaluate_function
#





#
# NOTICE
# ***
# Namespacing is no longer relevant, since we had to pull out for recursive depth anyway.
# Remove it at some point.
# ***
#
sub exec_lol
{
  my ($list,$namespace,$depth) = @_;
  if ( !defined $depth )
  {
    $depth = 0;
  }

  $breaking->[$depth] = 0;

  for my $n ( @$list )
  {
    #
    # early traps
    #
    if ( !$n )
    {
      next;
    }

    if ( $breaking->[$depth] )
    {
      last;
    }
    #
    # /traps
    #

    my $cmd = $n->{CMD};

    if ( $cmd eq 'IM_IN_YR' ) # loop
    {
      while ( 1 )
      {
        my $value = exec_lol($n->{LOOPEXEC},$namespace,$depth);
        if ( defined $value->{VALUE} or defined $value->{ARRAY} )
        {
          return $value;
        }

        if ( $breaking->[$depth] )
        {
          #
          # poorly-thought KW addition
          #
          $breaking->[$depth] = 0;
          last;
        }
      }
    }

    #
    # GTFO
    # -----
    # Loop-break statement
    #
    if ($cmd eq 'GTFO' )
    {
      $breaking->[$depth] = 1;
    }
    #
    # /GTFO
    #

    #
    # I HAS A
    # -----
    # Variable initializations
    #
    # NOTE
    # ***
    # Leaving T00B declarations in place for write access. However, they aren't really implemented anywhere.
    #
    if($cmd eq 'I_HAS_A')
    {
      if( defined $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{VAR}} )
      {
        die 'Double declaration of "'.$n->{'VAR'}.'" in "'.$namespace.'" on line '.$n->{LINE}.'. Aborting.'."\n";
      }
      elsif ( defined $fn_table->{$n->{VAR}} )
      {
        die 'Attempted redefinition of function "'.$n->{VAR}.'" as variable on line '.$n->{LINE}.'. Aborting.'."\n";
      }
      else
      {
        #
        # WARNING:
        # Deprecated behaviour.
        #
        if ( $n->{PIPE} )
        {
          my $location = $n->{VAL};
          $location =~ s/^"|"$//g;

          if ( $location =~ /^(https?):\/\//o )
          {
            # open a socket
          }
          else
          {
            open(LOCATION,$location);
            $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{VAR}}->[0]->{VALUE} = \*LOCATION;
          }
        }
        else
        {
          if ( defined $n->{VAL} )
          {
            if ( defined $n->{VAL}->{KW} )
            {
              set_variable($n->{VAR}->{ATOM},call_keyword($n->{VAL},$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE},1);
            }
            else
            {
              my $value = $n->{VAL}->{FN};
              if ( defined $fn_table->{$n->{VAL}->{FN}->{ATOM}}->{VARS} )
              {
                set_variable($n->{VAR}->{ATOM},evaluate_function($n->{VAL},$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE},1);
              }
              else
              {
                set_variable($n->{VAR}->{ATOM},evaluate_formula($value,$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE},1);
              }
            }
          }
          else
          {
            set_variable($n->{VAR}->{ATOM},array([]),$namespace,$depth,$n->{LINE},1);
          }
        }
      }
    }
    #
    # /I HAS A
    #

    #
    # LOL execution
    # -----
    # LOL sets a variable to a new value; arrays are passed by reference (evil).
    #
    if ( $cmd eq 'LOL' )
    {
      if ( defined $n->{VAL}->{KW} )
      {
        set_variable($n->{VAR},call_keyword($n->{VAL},$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE});
      }
      else
      {
        my $value = $n->{VAL}->{FN};
        if ( defined $fn_table->{$n->{VAL}->{FN}->{ATOM}}->{VARS} )
        {
          set_variable($n->{VAR},evaluate_function($n->{VAL},$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE});
        }
        else
        {
          set_variable($n->{VAR},evaluate_formula($value,$namespace,$depth,$n->{LINE}),$namespace,$depth,$n->{LINE});
        }
      }
    }
    #
    # /LOL
    #

    if($cmd eq 'UP')
    {
      if ( defined $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{VAR}->[0]} )
      {
        set_variable($n->{VAR},
                     {
                      ATOM => [
                        evaluate_formula(
                          {
                            LHS => {
                              ATOM => $n->{VAR}
                            },
                            OPR => 'UP',
                            RHS => {
                              ATOM => [
                                1
                              ]
                            }
                          },
                          $namespace,
                          $n->{LINE}
                        )
                      ]
                    },
                    $namespace,
                    $depth,
                    $n->{LINE}
        );
      }
      else
      {
        die 'Attempted increment of unknown variable "'.$n->{VAR}->[0].'" in "'.$namespace.'" on line '.$n->{LINE}.'. Aborting.'."\n";
      }
    }

    #
    # Comparators.
    #
    if ( $cmd eq 'SMALLER_THAN' or $cmd eq 'BIGGER_THAN' )
    {
      my $lhs = evaluate_formula($n->{LHS},$namespace,$depth,$n->{LINE});
      my $rhs = evaluate_formula($n->{RHS},$namespace,$depth,$n->{LINE});

      if ( !defined $lhs->{VALUE} or !defined $rhs->{VALUE} )
      {
        die sprintf('Error: malformed comparator on line %d. Aborting.'."\n",$n->{LINE});
      }

      $lhs = $lhs->{VALUE};
      $rhs = $rhs->{VALUE};

      if ( $lhs =~ /^\-?([0-9]+(\.[0-9]+)?|[0-9]*(\.[0-9]+)?)(e-?\d{1,3})?$/o and $rhs =~ /^\-?([0-9]+(\.[0-9]+)?|[0-9]*(\.[0-9]+)?)(e-?\d{1,3})?$/o )
      {
        if ( $cmd eq 'SMALLER_THAN' )
        {
          return value($lhs < $rhs ? 1 : 0);
        }
        else
        {
          return value($lhs > $rhs ? 1 : 0);
        }
      }
      else
      {
        if ( $cmd eq 'SMALLER_THAN' )
        {
          return value($lhs lt $rhs ? 1 : 0);
        }
        else
        {
          return value($lhs gt $rhs ? 1 : 0);
        }
      }
    }
    #
    # /Comparators.
    #

    #
    # VISIBLE
    # -----
    # Dumps to a pipe (defaults to STDOUT)
    #
    if($cmd eq 'VISIBLE')
    {
      my $f;
      if ( $n->{'DEST'} eq 'STDOUT' ) {
        $f = \*STDOUT;
      } else {
        $f = $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{'DEST'}}->[0]->{VALUE};
      }

      my $value = evaluate_formula($n->{VAR},$namespace,$depth,$n->{LINE});
      if ( defined $value->{ARRAY} )
      {
        print $f $value->{ARRAY};
      }
      else
      {
        $value->{VALUE} =~ s/^"|"$//g;
        print $f $value->{VALUE};
      }

      unless($n->{NOBREAK})
      {
        print $f "\n";
      }
    }
    #
    # /VISIBLE
    #

    #
    # IZ ORLY
    # -----
    # Conditional construct
    # 
    if ( $cmd eq 'IZ_ORLY' )
    {
      my $iz = exec_lol([$n->{IZEXEC}],$namespace,$depth);
      my $okay = $iz->{VALUE} or $iz->{ARRAY};
      
      if ( $okay )
      {
        if( $n->{YESEXEC} )
        {
          my $value = exec_lol($n->{YESEXEC},$namespace,$depth);
          if ( defined $value->{VALUE} or $value->{ARRAY} )
          {
            return $value;
          }
        }
      }
      else
      {
        if( $n->{NOEXEC} )
        {
          my $value = exec_lol($n->{NOEXEC},$namespace,$depth);
          if ( defined $value->{VALUE} or $value->{ARRAY} )
          {
            return $value;
          }
        }
      }
    }
    #
    # /IZ ORLY
    #

    #
    # IZ execution
    # -----
    # IZ is a one-line conditional, executing the command if value is true (watch this for bugs)
    #
    if ( $cmd eq 'IZ' )
    {
      my $iz = exec_lol([$n->{IZEXEC}],$namespace,$depth);
      if ( $iz->{VALUE} or $iz->{ARRAY} )
      {
        my $value = exec_lol([$n->{YESEXEC}],$namespace,$depth);
        if ( defined $value->{VALUE} or defined $value->{ARRAY} )
        {
          return $value;
        }
      }
    }
    #
    # /IZ
    #

    #
    # GIMMEH execution
    # -----
    # GIMMEH returns a line of input from a stream (auto-detection of http:// vs. file:// is on the way)
    #
    if ( $cmd eq 'GIMMEH' )
    {
      my $val = '';
      if ( $n->{SRC}->{ATOM}->[0] =~ /^STDIN$/i )
      {
        $val = <STDIN>;
      }
      else
      {
        # old declarations for pipe-based syntax
        #my $f = $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{SRC}}->[0]->{VALUE};
        #$val = <$f>;
        my $src;
        if ( !defined $n->{SRC}->{STR} )
        {
          $src = evaluate_formula($n->{SRC},$namespace,$depth,$n->{LINE});
        }
        else
        {
          $src = string_value($n->{SRC}->{ATOM},$namespace,$depth,$n->{LINE});
        }

        $src = $src->{VALUE};
        $src =~ s/^"|"$//g;
        if ( $src =~ /^https?:/i )
        {
          $val = get($src);
        }
        else
        {
          open(FILE,'<'.$src);
          while ( <FILE> )
          {
            $val .= $_;
          }
          close(FILE);
        }
      }

      chomp $val;
      my @v = split /[\r\n]+/, $val;

      if ( $n->{TYPE} eq 'NUMBR' )
      {
        if ( $v[0] =~ /^\-?([0-9]+(\.[0-9]+)?|[0-9]*(\.[0-9]+)?)(e-?\d{1,3})?$/o )
        {
          set_variable($n->{VAR},value($v[0]),$namespace,$depth,$n->{LINE});
        }
        else
        {
          die sprintf('Error: attempted entry of non-numeric value "%s" in "%s" at line %d. Aborting.'."\n",$v[0],$namespace,$n->{LINE});
        }
      }
      elsif ( $n->{TYPE} ne 'LINEZ' )
      {
        if ( $n->{TYPE} eq 'WURD' )
        {
          $v[0] =~ /(\w+)/;
          $v[0] = $1;
        }
        elsif ( $n->{TYPE} eq 'LETTAR' )
        {
          $v[0] = substr $v[0], 0, 1;
        }

        set_variable($n->{VAR},value($v[0]),$namespace,$depth,$n->{LINE});
      }
      else
      {
        for ( 0..$#v )
        {
          $v[$_] = value($v[$_]);
        }
        set_variable($n->{VAR},array(\@v),$namespace,$depth,$n->{LINE});
      }
    }
    #
    # /GIMMEH
    #

    #
    # DO NOT WANTZ execution
    # -----
    # DO NOT WANTZ frees pipe resources (sockets, filehandles, etc.)
    #
    if ( $cmd eq 'DO_NOT_WANTZ' )
    {
      my $f = $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{VAR}}->[0]->{VALUE};
      close($f) or die('Cannot close connection "'.$n->{'VAR'}.'" in "'.$namespace.'".');
      delete $fn_table->{$namespace}->{VARS}->[$depth]->{$n->{VAR}}->[0]->{VALUE};
    }
    #
    # /DO NOT WANTZ
    #
    
    #
    # LIEK
    # -----
    # Determines whether or not two formulas are the same
    #
    if ( $cmd eq 'LIEK' )
    {
      my $ret = 0;
      my $lhs = evaluate_formula($n->{LHS},$namespace,$depth,$n->{LINE});
      my $rhs = evaluate_formula($n->{RHS},$namespace,$depth,$n->{LINE});

      if ( ( defined $lhs->{VALUE} and !defined $rhs->{VALUE} ) or ( !defined $lhs->{VALUE} and defined $rhs->{VALUE} ) )
      {
        die 'Error: type mismatch in LIEK comparator at line '.$n->{LINE}.'. Aborting.'."\n";
      }

      if ( defined $lhs->{ARRAY} and $lhs->{ARRAY} eq $rhs->{ARRAY} )
      {
        $ret = 1;
      }
      if ( defined $lhs->{VALUE} )
      {
        if ( $lhs->{VALUE} =~ $number_match and $lhs->{VALUE} == $rhs->{VALUE} )
        {
          $ret = 1;
        }
        elsif ( $lhs->{VALUE} eq $rhs->{VALUE} )
        {
          $ret = 1;
        }
      }

      if ( $n->{NOT} )
      {
        $ret = 1 - $ret;
      }

      return value($ret);
    }
    #
    # /LIEK
    #

    #
    # SORTA
    # -----
    # Regex matching
    #
    if ( $cmd eq 'SORTA' )
    {
      my $ret = 0;
      my $lhs = evaluate_formula($n->{LHS},$namespace,$depth,$n->{LINE});
      my $rhs = string_value($n->{RHS},$namespace,$depth,$n->{LINE});

      $lhs = $lhs->{VALUE};
      $rhs = $rhs->{VALUE};
      $regex_matches = [];

      if ( $lhs =~ /$rhs/ )
      {
        $ret = 1;

        $regex_matches->[0] = $0;
        $regex_matches->[1] = $1;
        $regex_matches->[2] = $2;
        $regex_matches->[3] = $3;
        $regex_matches->[4] = $4;
        $regex_matches->[5] = $5;
        $regex_matches->[6] = $6;
        $regex_matches->[7] = $7;
        $regex_matches->[8] = $8;
        $regex_matches->[9] = $9;
      }

      return value($ret);
    }
    #
    # /SORTA
    #

    if ( $cmd eq 'FOUND_YR' )
    {
      if ( defined $n->{VAL}->{KW} )
      {
        return call_keyword($n->{VAL},$namespace,$depth,$n->{LINE});
      }

      my $value = $n->{VAL}->{FN};
      if ( defined $fn_table->{$value->{ATOM}}->{VARS} )
      {
        return evaluate_function($n->{VAL},$namespace,$depth,$n->{LINE});
      }

      return evaluate_formula($value,$namespace,$depth,$n->{LINE});
    }

    #
    # DIAF
    # -----
    # Die in a fire; die/exit functionality.
    #
    if ( $cmd eq 'DIAF' )
    {
      if ( defined $n->{FN} )
      {
        if ( defined $fn_table->{$n->{FN}->{FN}->{ATOM}}->{VARS} )
        {
          my $die_value = evaluate_function($n,$namespace,$depth,$n->{LINE});
          die $die_value->{VALUE}."\n";
        }
        
        my $die_value = evaluate_formula($n->{FN}->{FN},$namespace,$depth,$n->{LINE});
        die $die_value->{VALUE}."\n";
      }

      die 'Killing program at line '.$n->{LINE}.'.'."\n";
    }
    #
    # /DIAF
    #
  }

  my $return = $fn_table->{$namespace}->{VARS}->[$depth]->{IT};
  if ( defined $return )
  {
    return $return->[0];
  }

  return value;
}

sub showerrors
{
    foreach my $err (@{$parser->{errors}})
    {
        my $lineno    = $err->[1];
        my $errortext = $err->[0];
        print "Line $lineno: $errortext\n";
    }
    $parser->{errors} = undef;

    exit;
}

