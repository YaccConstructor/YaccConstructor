@echo off
del log1.txt
del log2.txt

for %%i in (CalcErrorAmb, ManyReductions, ErrorToEps, Ambiguous, EpsInTheEnd) do (
	..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -light true -o %%i.yrd.fs" >> log1.txt
)

for %%i in (PrimitiveErrorTranslate, ErrorToEpsilonTranslate, PrintErrorInfo) do (
	..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -o %%i.yrd.fs" >> log2.txt
)
