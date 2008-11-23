#!/bin/bash
exec="dist/build/tm/tm"
mama="./mama/mama"
stats="stats-dir"

if [ -e $stats ]
then
  echo "$stats already exists, please rm -r"
  exit
fi

mkdir $stats

for dir in `ls -d examples/*/. examples/.` 
do
  for file in `grep -l main $dir/*` 
  do 
    mkdir -p $stats/$dir
    $exec $@ -I $dir $file > $stats/$file
    echo $file >> $stats/stats.txt
    $mama -r -s $stats/$file >> $stats/stats.txt
  done
done

if [ -e stats.txt ]
then
  diff -ru -U 6 stats.txt $stats/stats.txt
else
  echo "Creating baseline."
  cp $stats/stats.txt .  
fi 

rm -r $stats

exit

