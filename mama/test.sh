#!/bin/bash

(for v in `ls test/*.cbn test/*.cbv` 
do 
  echo "$v"
  ./mama -r $v 
done) > rslt.txt

if (diff -q rslt.txt test/rslt.txt) > /dev/null 
then
  echo "all ok"
else
  echo "test failed:"
  diff -u  rslt.txt test/rslt.txt
fi 

rm rslt.txt