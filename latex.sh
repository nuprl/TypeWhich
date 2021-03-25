#!/bin/sed -f
s/fun/λ/g
s/any/\*/g
s/\.gtlc//g
s/_/ /g
s/ : /:/g
s/λ /λ/g
s/λ/\\lambda /g
s/->/\\rightarrow /g
s/BEGIN/\\begin{array}{l|l}/g
s/migeed/MaxMigrate/g
s/ins-and-outs/InsAndOuts/g
s/smt/Our tool/g
s/no-context/PreciseMigrate/g
