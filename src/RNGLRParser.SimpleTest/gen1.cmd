
del log.txt
            
                                   
..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe  -i BadLeftRecursion.yrd ^
           -g "RNGLRGenerator -pos int -token int -module RNGLR.BadLeftRecursion -o BadLeftRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleLeftRecursion.yrd ^
           -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleLeftRecursion -o SimpleLeftRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleRightRecursion.yrd ^
           -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleRightRecursion -o SimpleRightRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleAmb.yrd ^
           -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleAmb -o SimpleAmb.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i Mixed.yrd ^
           -g "RNGLRGenerator -pos int -token int -module RNGLR.Mixed -o Mixed.yrd.fs" >> log.txt


