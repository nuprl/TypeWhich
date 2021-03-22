#!/bin/sh

cd $(dirname $0)
[ -d grift-suite ] && [ -d migeed ] || {
    printf "Couldn't find test suite directories. Run $0 from the 'hm-in-smt' root or with a relative path.\n"
    exit 3
}

QUIET=

if [ "$1" = "-q" ]
then
    QUIET="-q"
    shift
fi

if [ "$1" = "grift" ]; then
    : ${HMSMT="target/debug/typeinf-playground"}
    : ${HMSMT_ARGS="-p grift"}
    : ${SUITE_DIR="grift-suite"}
    : ${EXT="*.grift"}
elif [ "$1" = "migeed-migeed" ]; then
    # i have tried to make this cleaner than this cd mess but stack doesn't
    # allow a lot of things on the command line and i don't wanna make a whole
    # package so...
    cd migeed
    export STACK_YAML="../../Maximal-Migration/stack.yaml"
    : ${HMSMT="stack runghc Migrate.hs"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="."}
    : ${EXT="*.gtlc"}
elif [ "$1" = "migeed-ins-and-outs" ]; then
    : ${HMSMT="ins-and-outs/target/debug/ins-and-outs"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="migeed"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "migeed-smt" ]; then
    : ${HMSMT="target/debug/typeinf-playground"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="migeed"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "migeed-no-context" ]; then
    : ${HMSMT="target/debug/typeinf-playground"}
    : ${HMSMT_ARGS="--unsafe"}
    : ${SUITE_DIR="migeed"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "adversarial-migeed" ]; then
    # i have tried to make this cleaner than this cd mess but stack doesn't
    # allow a lot of things on the command line and i don't wanna make a whole
    # package so...
    cd migeed
    export STACK_YAML="../../Maximal-Migration/stack.yaml"
    : ${HMSMT="stack runghc Migrate.hs"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="../adversarial"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "adversarial-ins-and-outs" ]; then
    : ${HMSMT="ins-and-outs/target/debug/ins-and-outs"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="adversarial"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "adversarial-smt" ]; then
    : ${HMSMT="target/debug/typeinf-playground"}
    : ${HMSMT_ARGS=""}
    : ${SUITE_DIR="adversarial"}
    : ${EXT="*.gtlc"}
elif [ "$1" = "adversarial-no-context" ]; then
    : ${HMSMT="target/debug/typeinf-playground"}
    : ${HMSMT_ARGS="--unsafe"}
    : ${SUITE_DIR="adversarial"}
    : ${EXT="*.gtlc"}
else
    printf "Usage: $(basename $0) [-q] [grift|migeed-ins-and-outs|migeed-context|migeed-smt]\n\n"
    printf "\t-q\tquiet mode (only show failures)\n"
    exit 2
fi

shift

if [ "$1" = "--print-types" ]; then
    : ${PRINT_TYPES=1}
    echo "PRINTING TYPES"
    shift
fi

total=0
failures=0

last_group=""

if [ "$*" ]
then
    PAT="$1"
    shift
    for kw in "$@"
    do
        PAT="$PAT\|$kw"
    done
    files=$(find "$SUITE_DIR" -name "$EXT" | grep -e "$PAT")
else
    files=$(find "$SUITE_DIR" -name "$EXT")
fi

first_header=1
needs_header=
header() {
    if ! [ "$first_header" ]
    then
        printf "\n"
    fi
    first_header=
    
    printf "\033[1m$test_group\033[0m\n"
    printf "=================================\n"
}

for test_file in $files
do
    test_name=$(basename $test_file)
    test_name=${test_name%.grift}

    test_group=$(dirname $test_file)
    test_group=${test_group#./}

    if ! [ "$test_group" = "$last_group" ]
    then
        
        last_group=$test_group
        if [ "$QUIET" ]
        then
            needs_header=1
        else
            header
        fi
    fi

    if ! [ "$QUIET" ]
    then
        printf "%24s..." "$test_name"
    fi

    OUT="$(dirname $test_file)/$test_name.out"
    ERR="$(dirname $test_file)/$test_name.err"
    timeout 5 $HMSMT $HMSMT_ARGS $test_file >$OUT 2>$ERR
    status=$?
    : $((total += 1))
    if [ $status -eq 0 ]
    then
        rm $ERR
        if ! [ "$QUIET" ]
        then
           printf "....\033[32mOK\033[0m"
        fi
    else
        : $((failures += 1))
        if [ "$QUIET" ]
        then
            if [ "$needs_header" ]
            then
                header
                needs_header=
            fi
            
            printf "%24s..." "$test_name"
        fi            
        printf "\033[31mFAILED\033[0m"
    fi
    if [ "$PRINT_TYPES" ] && [ $status -eq 0 ]; then
        printf "    "
        tail -n 1 $OUT
    else
        printf "\n"
    fi
done

printf "=================================\n"

printf "\033[1m%d\033[0m/\033[1m%d\033[0m passed" \
       $((total - failures)) $total
if [ $failures -eq 0 ]
then
    printf " (\033[32mPASSED\033[0m)\n"
else
    printf " (\033[31m%d\033[0m failures)\n" $failures
fi


[ $failures -eq 0 ]
