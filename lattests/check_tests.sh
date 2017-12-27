#!/bin/bash
counter=0
counter1=0

shopt -s nullglob
for i in $PWD/lattests/bad/*.lat; do
    filename="${i##*/}"
    echo "Checking incorrect file: $filename: "
    ./latc_llvm lattests/bad/$filename
    echo
    fname="${filename%.*}"
    fname="$fname.ll"
    line=$(head -n 1 lattests/bad/$fname 2>/dev/null)
    if [ "$line" == "Ok" ];
    then
    	counter=$((counter+1))
    fi
done

for i in $PWD/lattests/good/*.lat; do
    filename="${i##*/}"
    echo "Checking correct file: $filename: "
    ./latc_llvm lattests/good/$filename
    echo
    fname="${filename%.*}"
    fname="$fname.ll"
    line=$(head -n 1 lattests/good/$fname 2>/dev/null)
    if [ "$line" == "Ok" ];
    then
    	counter1=$((counter1+1))
    fi
done

let "rr = 60 - $counter"
echo
echo Testy bad '-->' '('poprawne/wszystkie')' $rr / 60
echo Testy good '-->' '('poprawne/wszystkie')' $counter1 / 22

rm -f lattests/bad/*.ll
rm -f lattests/good/*.ll