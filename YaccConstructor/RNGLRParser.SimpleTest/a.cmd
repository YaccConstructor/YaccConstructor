del out

for %%i in (Longest) do (
    ..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd -c ExpandEbnf -c ExpandMeta -g YardPrinter >> out
)

