#!/bin/bash
# don't worry first time here
counter=0
counter1=0

shopt -s nullglob
for i in $PWD/lattests/bad/*.lat; do
    filename="${i##*/}"
    echo "Checking incorrect file: $filename: "
    ./latc_llvm lattests/bad/$filename
    fname="${filename%.*}"
    fname="$fname.ll"
    if [! -f lattests/bad/$fname  ];
    then
    	counter=$((counter+1))
    fi
    echo
done

for i in $PWD/lattests/good/*.lat; do
    filename="${i##*/}"
    echo "Checking correct file: $filename: "
    ./latc_llvm lattests/good/$filename
    fname="${filename%.*}"
    fname2="$fname.bc"
    fname3="$fname.output"
    fname4="$fname.output2"
    lli lattests/good/$fname2 > lattests/good/$fname4
    if diff -q lattests/good/$fname4 lattests/good/$fname3; then
    	counter1=$((counter1+1))
    fi
    echo
done

let "rr = 65 - $counter"

echo
echo Testy bad '-->' '('poprawne/wszystkie')' $rr / 65
echo Testy good '-->' '('poprawne/wszystkie')' $counter1 / 37

rm -f lattests/bad/*.ll
rm -f lattests/bad/*.bc
# rm -f lattests/good/*.ll
rm -f lattests/good/*.bc
rm -f lattests/good/*.output2