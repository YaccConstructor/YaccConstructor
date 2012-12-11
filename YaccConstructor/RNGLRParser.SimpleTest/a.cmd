for %%i in (InfEpsilon) do (
    ..\Main\bin\Release\YaccConstructor.exe -i %%i.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LR -light on -o %%i.yrd.fs"
)

