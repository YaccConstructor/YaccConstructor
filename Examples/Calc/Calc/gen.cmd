@echo off
..\yc\YaccConstructor.exe -f YardFrontend ^
    -c ExpandMeta -c ExpandEbnf ^
    -g "RNGLRGenerator -token string -module Calc.Parse -o Parser.fs" ^
    -i calc.yrd > log.txt