#!/bin/ksh
# Run under ksh
 
#  ------------------------------------------------------------------------
#  This script will FORTRAN block or unblock BUFR files on a number of
#  standard computing platforms. Stictly speaking, real BUFR files are
#  unblocked, that is, a byte stream containing only allowable BUFR
#  constructs. On some platforms it is advantagous to use the FORTRAN
#  blocked structure for I/O efficiency, and on some platforms, when
#  using FORTRAN I/O, the unblocked structure is FORTRAN UN-readable.
#  NOTE: The script is set up to run in the Bourne shell. If you are a
#  C-shell user, enter 'sh ./bufr_cword.sh'.
#  ------------------------------------------------------------------------
#  bufr_cword.sh: <action> <inputfile> <outputfile>
#
#  where:
#
#  <action>     can be block or unblk
#  <inputfile>  [path/]filename of input file
#  <outputfile> [path/]filename of output file
#  ------------------------------------------------------------------------

set -x

[ $# -ne 3 ] && { echo; echo "$0: <action> <inputfile> <outputfile>";
                  echo;
                  echo "where:";
                  echo;
                  echo "<action>     can be block or unblk";
                  echo "<inputfile>  [path/]filename of input file";
                  echo "<outputfile> [path/]filename of output file";
                  echo; exit 99; }

CWORDX=${CWORDX:-$EXECobsproc_shared_bufr_cword/bufr_cword}

cat<<eof|$CWORDX
$1
$2
$3
eof
err=$?
exit $err
