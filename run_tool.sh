if [ "$1" = "migeed" ]; then
    ./bin/MaxMigrate $2
elif [ "$1" = "ins-and-outs" ]; then
    target/debug/typeinf-playground --ins-and-outs $2 | tr '\n' ' '
elif [ "$1" = "smt" ]; then
    target/debug/typeinf-playground $2 | tr '\n' ' '
    echo
elif [ "$1" = "no-context" ]; then
    target/debug/typeinf-playground --unsafe $2 | tr '\n' ' '
    echo
elif [ "$1" = "grift" ]; then
    target/debug/typeinf-playground -p grift $2
else
    echo "unknown tool"
fi
