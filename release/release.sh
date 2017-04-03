#!/bin/bash

cp ../.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/connect-four-exe/connect-four-exe .
cp -r ../asset/ .
cp ../README.md .
readonly FILES="asset/ connect-four-exe README.md"
zip -r connect-four.zip $FILES
rm -r $FILES
