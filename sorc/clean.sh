#!/bin/sh
set -x
set -e    # fail if an error is hit so that errors do not go unnoticed

if [ $# -eq 0 ]; then
  # require at least 1 arg
  set +x
  echo "$0 requires at least one argument"
  echo "usage: $0 <some_code.fd ...>"
  set -x
  exit
else
  dir_list=$*
fi

for sdir in $dir_list;do
 dir=${sdir%\/}  # chop trailing slash if necessary
 cd $dir
 make clean
 ls -l
 cd ..
done


