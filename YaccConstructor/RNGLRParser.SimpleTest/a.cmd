for %%i in (Calc) do (
    ..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LR -light on -o %%i.yrd.fs" >> log.txt
)

