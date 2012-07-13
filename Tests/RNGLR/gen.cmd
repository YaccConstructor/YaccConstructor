@echo off

for %%i in (Attrs,Calc,Counter,Cycle) do (
    ..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -i %%i\%%i.yrd -g "RNGLRGenerator -token int -module RNGLR.Parse%%i -translate true" > %%i\log_%%i.txt
    copy %%i\%%i.yrd.fs ..\..\YaccConstructor\RNGLRParser.SimpleTest\%%i.yrd.fs
)

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
    ..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -i %%i\%%i.yrd -g "RNGLRGenerator -token int -module RNGLR.Parse%%i -translate false" > %%i\log_%%i.txt
    copy %%i\%%i.yrd.fs ..\..\YaccConstructor\RNGLRParser.SimpleTest\%%i.yrd.fs
)
