::@echo off
del log1.txt
del log2.txt

for %%i in (Elementary, Ambiguous, Cycles, Brackets_1, Brackets_2, Brackets_3) do (
	echo.  >> log1.txt
	echo %%i >> log1.txt
	..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -light true -o %%i.yrd.fs" >> log1.txt
)
         
