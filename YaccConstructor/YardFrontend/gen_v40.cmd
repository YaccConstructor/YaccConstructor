@echo off

..\packages\YC.FsYacc_RNGLR.0.0.5.3\tools\YaccConstructor.exe   -f FsYaccFrontend -i Parser.fsy ^
    -g "RNGLRGenerator -o Parser.fs -module Yard.Frontends.YardFrontend.GrammarParser -pos Source.Position -token Source.t" > log.txt