::@echo off
del log1.txt
del log2.txt

for %%i in (CalcErrorAmb, ManyReductions, ErrorToEps, Ambiguous) do (
	echo.  >> log1.txt
	echo %%i >> log1.txt
	..\packages\YaccConstructor.0.0.8.12\tools\YaccConstructor\YC.YaccConstructor.exe  -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -light true -o %%i.yrd.fs" >> log1.txt
)

for %%i in (PrimitiveErrorTranslate, ErrorToEpsilonTranslate, PrintErrorInfo, PrintErrorInfoEOF) do (
	echo.  >> log2.txt
	echo %%i >> log2.txt
	..\packages\YaccConstructor.0.0.8.12\tools\YaccConstructor\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -o %%i.yrd.fs" >> log2.txt
)