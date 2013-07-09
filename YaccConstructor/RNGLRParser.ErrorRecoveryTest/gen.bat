for %%i in (CalcErrorAmb, ManyReductions, ErrorToEps) do (
	..\YaccConstructor\bin\Release\YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -o %%i.yrd.fs" > log%%i.txt
)