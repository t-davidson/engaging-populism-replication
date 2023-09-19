#!/bin/bash
#
FILES="*.R"
I=73
for f in $FILES
do
  #echo "Processing $f file..."
  #echo " $I "
  cp ${f} "../renamed_files/script-${I}.R"
  I=$[$I +1]
done
