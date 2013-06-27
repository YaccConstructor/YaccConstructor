@echo off
..\yc\YaccConstructor.exe -f YardFrontend ^

    -g "RNGLRGenerator  -o Parser.fs" ^
    -i calc.yrd > log.txt