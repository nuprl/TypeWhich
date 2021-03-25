#!/bin/sh

cd $(dirname $0)

files=$(ls grift-suite/benchmarks/src/dyn/*.grift)

echo 'BEGIN'
for test_file in $files
do
    out=$(timeout 10 ./run_tool.sh grift "$test_file" 2>/dev/null)
    status=$?
    if [ $status -ne 0 ]
    then
        out="\\\textrm{No migration produced}"
    else
        out="\\\texttt{$out}"
    fi
    test_name=$(basename $test_file .grift | sed 's/[_-]/ /g')
    echo "\\\textrm{$test_name}" "&" "$out" '\\\\'
done
echo '\\end{array}\n\n'
