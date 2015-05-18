::@echo off
del log.txt

for %%i in (ExtendedCalc, If) do (
	echo.  >> log.txt
	echo %%i >> log.txt
	..\..\Bin\Debug\v40\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -light true -o %%i.yrd.fs" >> log.txt
)
         
