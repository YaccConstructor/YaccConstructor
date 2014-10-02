::@echo off
del log1.txt

for %%i in (Elementary, Ambiguous, Cycles, Summator) do (
	echo.  >> log1.txt
	echo %%i >> log1.txt
	..\..\Bin\Release\v40\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -highlighting true -namespace %%i -light true -o %%i.yrd.fs" >> log1.txt
)
         
