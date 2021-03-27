#!/bin/sh

cd $(dirname $0)

files=$(ls grift-suite/benchmarks/src/dyn/*.grift)

for test_file in $files
do
    basename=$(basename $test_file .grift)
    dynamic_version="grift-suite/benchmarks/src/static/$basename/single/$basename.grift"
    echo $basename $(target/debug/typeinf-playground -p grift "$test_file" --compare "$dynamic_version")
done
