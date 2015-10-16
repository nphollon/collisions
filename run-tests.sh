#/usr/bin/env bash

elm_out="$TMPDIR/test-raw.js"
io_out="$TMPDIR/test.js"
io_sh="elm-stuff/packages/maxsnew/IO/1.0.1/elm-io.sh"
main_elm="test/Main.elm"

elm make $main_elm --output $elm_out --yes

if [ $? -ne 0 ]; then exit 1; fi;

sh $io_sh $elm_out $io_out
if [ $? -ne 0 ]; then exit 1; fi;

echo "Successfully generated $io_out"

node $io_out
