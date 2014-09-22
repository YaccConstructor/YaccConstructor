::@echo off
del log1.txt

for %%i in (Elementary, Ambiguous, Cycles, Summator) do (
	echo.  >> log1.txt
	echo %%i >> log1.txt
	..\packages\YaccConstructor.0.0.8.12\tools\YaccConstructor\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -highlighting true -namespace %%i -light true -o %%i.yrd.fs" >> log1.txt
)
         
