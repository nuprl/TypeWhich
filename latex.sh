#!/bin/sed -f
s/fun/λ/g
s/any/\\dyn/g
s/\.gtlc//g
s/_/ /g
s/ : /:/g
s/λ /λ/g
s/λ/\\kw{fun} /g
s/->/\\rightarrow /g
s/BEGIN/\\begin{array}{l|l}/g
s/migeed/MaxMigrate/g
s/ins-and-outs/InsAndOuts/g
s/smt/\\ourtool/g
s/no-context/PreciseMigrate/g
