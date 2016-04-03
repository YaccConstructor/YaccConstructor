::@echo off
del log.txt

for %%i in (Elementary, Ambiguous, Cycles, Summator, Summator2) do (
	echo.  >> log.txt
	echo %%i >> log.txt
	..\..\Bin\Release\v40\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -namespace %%i -light true -abstract true -o %%i.yrd.fs" >> log.txt
)
         
