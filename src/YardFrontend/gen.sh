#!/usr/bin/env bash

mono ../../Bin/Release/v40/YC.FsLex.exe --unicode -o Lexer.fs lexer.fsl
echo > log.txt
fsi gen.fsx > log.txt