#!/bin/sh

: ${HMSMT="../target/debug/typeinf-playground"}
: ${HMSMT_ARGS="-p grift"}

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
    files=$(find . -name \*.grift | grep -e "$PAT")
else
    files=$(find . -name \*.grift)
fi

for test_file in $files
do
    test_name=$(basename $test_file)
    test_name=${test_name%.grift}

    test_group=$(dirname $test_file)
    test_group=${test_group#./}

    if ! [ "$test_group" = "$last_group" ]
    then
        if [ "$last_group" ]
        then
            printf "\n"
        fi
        
        last_group=$test_group
        printf "\033[1m$test_group\033[0m\n"
        printf "=================================\n"
    fi
    
    printf "%24s..." "$test_name"

    OUT="$(dirname $test_file)/$test_name.out"
    ERR="$(dirname $test_file)/$test_name.err"
    $HMSMT $HMSMT_ARGS $test_file >$OUT 2>$ERR
    status=$?
    : $((total += 1))
    if [ $status -eq 0 ]
    then
        rm $ERR
        printf "....\033[32mOK\033[0m"
    else
        : $((failures += 1))
        printf "\033[31mFAILED\033[0m"
    fi
    printf "\n"
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
