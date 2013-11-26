@echo off

..\..\bin\Release\v45\YaccConstructor.exe  -f FsYaccFrontend -i Parser.fsy ^
    -g "RNGLRGenerator -o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos Source.Position -token Source.t" > log.txt