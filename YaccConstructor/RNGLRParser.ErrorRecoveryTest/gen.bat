@echo off

..\YaccConstructor\bin\Release\YaccConstructor.exe -i TrivialRecovery.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseTrivialRecovery -translate false -light on -o TrivialRecovery.yrd.fs" > log1.txt
		
..\YaccConstructor\bin\Release\YaccConstructor.exe -i calcErrorNonAmb.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseCalcErrorNonAmb -translate false -light on -o CalcErrorNonAmb.yrd.fs" > log2.txt

..\YaccConstructor\bin\Release\YaccConstructor.exe -i calcErrorAmb.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseCalcErrorAmb -translate false -light on -o CalcErrorAmb.yrd.fs" > log3.txt