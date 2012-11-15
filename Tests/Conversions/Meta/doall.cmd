@echo off
set fileName=_out

del _out

for /R %%i in (meta_*.yrd) do (
	cat %%i >> %fileName%
	@echo: >> %fileName%
	@echo ------------------------------------------------ >> %fileName%
	@echo: >> %fileName%
	..\..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -f YardFrontend -g YardPrinter -c ExpandMeta -i %%i >> %fileName%
	@echo: >> %fileName%
	@echo ================================================ >> %fileName%
	@echo: >> %fileName%
)

