@echo off

for /R %%i in (meta_*.yrd) do (
    echo %%i
    ..\..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -f YardFrontend -g YardPrinter -c ExpandMeta -i %%i > %%i.ans
)

