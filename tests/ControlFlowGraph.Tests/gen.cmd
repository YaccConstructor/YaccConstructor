::@echo off
del log.txt

for %%i in (ExtendedCalc, If, Simple, Let) do (
    echo.  >> log.txt
    echo %%i >> log.txt
    ..\..\bin\Release\v40\YC.AbstractLexer.Generator.exe %%iLexer.fsl --unicode -o %%iTest.Lexer.fs --abstract
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -module %%iTest.Parser -translate false -light true -abstract true -o %%i.yrd.fs" >> log.txt
)