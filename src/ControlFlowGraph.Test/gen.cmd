::@echo off
del log.txt

call gen_lex.cmd

for %%i in (ExtendedCalc, If, Simple) do (
	echo.  >> log.txt
	echo %%i >> log.txt
	..\..\Bin\Release\v40\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -module Test.%%iParser -translate false -light true -abstract true -o %%i.yrd.fs" >> log.txt
)
         
