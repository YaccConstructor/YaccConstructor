@echo off
set fileName=_out

del _out

for /R "../../../../Tests/Convertions/Meta/" %%i in (meta_*.yrd) do (
	cat %%i >> %fileName%
	@echo: >> %fileName%
	@echo ------------------------------------------------ >> %fileName%
	@echo: >> %fileName%
	YaccConstructor.exe -f YardFrontend -g YardPrinter -c ExpandMeta -i %%i >> %fileName%
	@echo: >> %fileName%
	@echo ================================================ >> %fileName%
	@echo: >> %fileName%
)

