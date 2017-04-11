..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalcWithErrors.yrd ^
        -g "RNGLRGenerator -pos int -token string -module SimpleCalcWithErrors -translate true -bindSrc false -o SimpleCalcWithErrors.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalcWithoutErrors.yrd ^
        -g "RNGLRGenerator -pos int -token string -module SimpleCalcWithoutErrors -translate true -bindSrc false -o SimpleCalcWithoutErrors.yrd.fs -abstract true" >> log.txt

