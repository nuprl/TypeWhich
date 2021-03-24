#!/bin/sed -f
s/fun/λ/g
s/any/\*/g
s/\.gtlc//g
s/_/ /g
s/ : /:/g
s/λ /λ/g
s/λ/$\\lambda$/g
s/->/$\\rightarrow$/g
s/BEGIN/\\begin{tabular}{p{0.16\\linewidth} | p{0.84\\linewidth}}/g
s/migeed/Migeed et al/g
s/ins-and-outs/Rastogi et al/g
s/smt/Our tool/g
s/no-context/Our most precise type/g
