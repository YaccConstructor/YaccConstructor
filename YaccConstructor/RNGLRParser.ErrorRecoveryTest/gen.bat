..\YaccConstructor\bin\Release\YaccConstructor.exe -i calcErrorAmb.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseCalcErrorAmb -translate false -o CalcErrorAmb.yrd.fs" > log.txt
		
..\YaccConstructor\bin\Release\YaccConstructor.exe -i ManyReductions.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseManyReductions -translate false -o ManyReductions.yrd.fs" > log1.txt
		
..\YaccConstructor\bin\Release\YaccConstructor.exe -i ManyReductions2.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseManyReductions2 -translate false -o ManyReductions2.yrd.fs" > log2.txt