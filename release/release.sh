#!/bin/bash

cp ../.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/connect-four-exe/connect-four-exe .
cp -r ../asset/ .
zip -r connect-four.zip asset/ connect-four-exe
rm -r asset/ connect-four-exe
