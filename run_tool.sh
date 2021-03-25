if [ "$1" = "migeed" ]; then
    ./bin/MaxMigrate $2
elif [ "$1" = "ins-and-outs" ]; then
    ins-and-outs/target/debug/ins-and-outs $2
elif [ "$1" = "smt" ]; then
    target/debug/typeinf-playground $2
elif [ "$1" = "no-context" ]; then
    target/debug/typeinf-playground --unsafe $2
elif [ "$1" = "grift" ]; then
    target/debug/typeinf-playground -p grift $2
else
    echo "unknown tool"
fi
