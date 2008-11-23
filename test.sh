#!/bin/bash
exec="dist/build/tm/tm"
test="test-dir"

if [ -e $test ]
then
  echo "$test already exists, please rm -r"
  exit
fi

mkdir $test

for dir in `ls -d examples/*/. examples/.` 
do
  for file in `grep -l main $dir/*` 
  do 
    mkdir -p $test/$dir
    $exec $@ -I $dir $file > $test/$file
    echo $file >> $test/$dir/result.txt
    ./mama/mama -r $test/$file >> $test/$dir/result.txt
  done
  
  diff -ru $dir/result.txt $test/$dir/result.txt
done

rm -r $test
echo "done. "
exit

