@echo off

..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -i Epsilon\Epsilon.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseEpsilon -translate true -table LR -o Epsilon\Epsilon.yrd.fs" > Epsilon\log_Epsilon.txt
copy Epsilon\Epsilon.yrd.fs ..\..\YaccConstructor\RNGLRParser.SimpleTest\Epsilon.yrd.fs 

for %%i in (Cond,Attrs,Calc,Counter,Cycle, Resolvers) do (
    ..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -i %%i\%%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LR -o %%i\%%i.yrd.fs" > %%i\log_%%i.txt
    copy %%i\%%i.yrd.fs ..\..\YaccConstructor\RNGLRParser.SimpleTest\%%i.yrd.fs
)

copy Calc\Calc.yrd.fs ..\..\YaccConstructor\RNGLRApplication\Calc.yrd.fs 

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
    ..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -i %%i\%%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -table LR -o %%i\%%i.yrd.fs" > %%i\log_%%i.txt
    copy %%i\%%i.yrd.fs ..\..\YaccConstructor\RNGLRParser.SimpleTest\%%i.yrd.fs
)

