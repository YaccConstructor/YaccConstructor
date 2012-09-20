@echo off

del log.txt

for %%i in (Epsilon, Longest) do (
    ..\Main\bin\Release\YaccConstructor.exe -i %%i.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)

for %%i in (Cond,Attrs,Calc,Counter,Cycle, Resolvers) do (
    ..\Main\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
    ..\Main\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -table LR -o %%i.yrd.fs" >> log.txt
)

