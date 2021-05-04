#!/bin/sh
# The programs with static types are very well annotated, but do not have
# annotations everywhere that they could. We assume that our annotations match
# for these terms.

cd $(dirname $0)

files=$(ls grift-suite/benchmarks/src/dyn/*.grift)

for test_file in $files
do
    basename=$(basename $test_file .grift)
    dynamic_version="grift-suite/benchmarks/src/static/$basename/single/$basename.grift"
    echo $basename $(./bin/TypeWhich migrate -p grift "$test_file" --compare "$dynamic_version")
done
