This directory contains the [grift test
suite](https://github.com/Gradual-Typing/Grift/tree/master/tests/suite)
as of 2021-03-14.

We use our own runner---we're just inferring types, not running the
actual programs. From this directory, run `run.sh` to run the tests.

# Notes

Some files are sitting around unused: those ending in `.rx` are
regular expressions for expected output from running the program;
those ending in `.in` are the supplied input. Since we're not running
programs, these files are irrelevant. We've held on to them in case we
ever want to run the programs.

We moved `simple-map.grift` into `program` to have neater output.
