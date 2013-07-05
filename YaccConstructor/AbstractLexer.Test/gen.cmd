@echo off
..\YaccConstructor\bin\Release\YaccConstructor.exe -f YardFrontend ^

    -g "RNGLRGenerator  -o Parser.fs" ^
    -i calc.yrd > log.txt