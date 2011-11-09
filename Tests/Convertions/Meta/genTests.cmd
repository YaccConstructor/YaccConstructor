@echo off

for /R "../../../../Tests/Convertions/Meta/" %%i in (meta_*.yrd) do (
	YaccConstructor.exe -f YardFrontend -g YardPrinter -c ExpandMeta -i %%i > %%i.ans
)

