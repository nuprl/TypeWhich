#!/bin/sh

cd $(dirname $0)
[ -d grift-suite ] && [ -d migeed ] || {
    printf "Couldn't find test suite directories. Run $0 from the 'hm-in-smt' root or with a relative path.\n"
    exit 3
}

if [ "$1" = "migeed" ]; then
    echo "MIGEED BENCHMARKS\n"
    : ${SUITE_DIR="migeed"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "adversarial" ]; then
    echo "ADVERSARIAL EXAMPLES\n"
    : ${SUITE_DIR="adversarial"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "all" ]; then
    ./tables.sh migeed
    ./tables.sh adversarial
    exit 0
fi

echo 'BEGIN'
tools="migeed ins-and-outs no-context"
files=$(ls $SUITE_DIR/$EXT)

for test_file in $files
do
    for tool in $tools; do
        out=$(timeout 10 ./run_tool.sh $tool $test_file 2>/dev/null)
        status=$?
        if [ $status -ne 0 ]
        then
            out="\\\textrm{No migration produced}"
        else
            out=$(echo "$out" | tail -n 1)
        fi
        echo "\\\textrm{$tool}" "&" "$out" '\\\\'
    done
    echo "\\\hline"
done
echo '\\end{array}\n\n'
