::@echo off
del log.txt

for %%i in (ExtendedCalc, If, Simple, Let) do (
    echo.  >> log.txt
    echo %%i >> log.txt
    ..\..\bin\Release\v40\YC.AbstractLexer.Generator.exe %%iLexer.fsl --unicode -o %%iTest.Lexer.fs --abstract
)
fsi gen.fsx >>log.txt