@echo off

del log.txt

for %%i in (Calc) do (
    ..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int*int -module RNGLR.Parse%%i -translate true -light on -o %%i.yrd.fs" >> log.txt
)
