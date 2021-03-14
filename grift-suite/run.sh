#!/bin/sh

: ${HMSMT="../target/debug/typeinf-playground"}
: ${HMSMT_ARGS="-p grift"}

total=0
failures=0

last_group=""
for test_file in $(find . -name \*.grift)
do
    test_name=$(basename $test_file)
    test_name=${test_name%.grift}

    test_group=$(dirname $test_file)
    test_group=${test_group#./}


    if ! [ "$test_group" = "$last_group" ]
    then
        last_group=$test_group
        printf "\n\033[1m$test_group\033[0m\n"
        printf "=================================\n"
    fi
    
    printf "%24s..." "$test_name"

    $HMSMT $HMSMT_ARGS $test_file >$(dirname $test_file)/$test_name.out 2>$(dirname $test_file)/$test_name.err
    status=$?
    : $((total += 1))
    if [ $status -eq 0 ]
    then
        printf "....\033[32mOK\033[0m"
    else
        : $((failures += 1))
        printf "\033[31mFAILED\033[0m"
    fi
    printf "\n"
done

printf "%3d/%3d passed (%3d failures)\n" $((total - failures)) $total $failures

[ $failures -eq 0 ]
