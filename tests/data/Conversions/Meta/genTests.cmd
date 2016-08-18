@echo off

for /R %%i in (meta_*.yrd) do (
    echo %%i
    ..\..\..\YaccConstructor\YaccConstructor\bin\Release\YaccConstructor.exe -f YardFrontend -g YardPrinter -c ExpandMeta -i %%i > %%i.ans
)

