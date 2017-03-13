#!/usr/bin/env bash

mono ../../Bin/Release/v40/YC.FsLex.exe --unicode -o Lexer.fs lexer.fsl
echo > log.txt
mono ../../Bin/Release/v40/YC.YaccConstructor.exe -f FsYaccFrontend -i parser.fsy -g "RNGLRGenerator -o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos Source.Position -token Source.t" > log.txt