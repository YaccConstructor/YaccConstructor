del log1.txt
del log2.txt

for %%i in (CalcErrorAmb, ManyReductions, ErrorToEps, Ambiguous) do (
	..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -o %%i.yrd.fs" >> log1.txt
)

for %%i in (PrimitiveErrorTranslate, ErrorToEpsilonTranslate) do (
	..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -o %%i.yrd.fs" >> log2.txt
)
