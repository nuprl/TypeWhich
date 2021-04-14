#!/bin/bash
set -e

echo "Running all benchmark programs (challenge, migeed, and grift)."
echo "Use time to measure total runtime"
echo "Total LOC:"
cat migeed/*.gtlc adversarial/*.gtlc grift-suite/benchmarks/src/dyn/*.grift | wc -l


TYPEWHICH=./target/release/typeinf-playground

for F in adversarial/*.gtlc; do
  $TYPEWHICH migrate $F > /dev/null
done  

for F in migeed/*.gtlc; do
  $TYPEWHICH migrate $F > /dev/null
done  

for F in grift-suite/benchmarks/src/dyn/*.grift; do
  $TYPEWHICH migrate -p grift $F > /dev/null
done