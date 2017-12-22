#!/bin/bash

shopt -s nullglob
for i in $PWD/lattests/bad/*.lat; do
    filename="${i##*/}"
    echo "checking file: $filename: "
    ./latc_llvm lattests/bad/$filename
done

rm -f lattests/bad/*.ll