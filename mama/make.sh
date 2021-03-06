#!/bin/bash


EXE=""

if [ -n "$WINDIR"  ] 
then 
  EXE=.exe
fi


RESULT=mama$EXE


# proccessing arguments
FAST=0
for arg in "$@"
do
  case "$arg" in
    notest)  FAST=1
             ;;
    fast)    FAST=2
             ;; 
    nostats) FAST=3
             ;;
    *)       ;;
  esac
done

if (which dmd$EXE > /dev/null) 
then
  FLAG="-inline -O -of$RESULT" 

  case "$FAST" in
    0) FLAG="$FLAG -unittest" ;;
    1) FLAG="$FLAG" ;;
    2) FLAG="$FLAG -release" ;;
    3) FLAG="$FLAG -release -version=NoStats" ;;
  esac
  
  echo "dmd$EXE $FLAG main.d"
  dmd$EXE $FLAG main.d
  if [ $? ] ; then exit $? ; fi
fi


if (which gdc > /dev/null) 
then 
  FLAG="-m32 -finline-functions -O2 -o $RESULT"

  case "$FAST" in
    0) FLAG="$FLAG -funittest" ;;
    1) FLAG="$FLAG" ;;
    2) FLAG="$FLAG -frelease" ;;
    3) FLAG="$FLAG -frelease -fversion=NoStats" ;;
  esac
  
  echo gdc $FLAG main.d
  (gdc $FLAG main.d 2>&1) > /dev/null

  if [ $? ] ; then exit $? ; fi
fi

