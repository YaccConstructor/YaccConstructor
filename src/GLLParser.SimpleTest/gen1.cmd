
del log.txt
                                                                                               
..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe  -i BadLeftRecursion.yrd ^
           -g "GLLGenerator -pos int -token int -module GLL.BadLeftRecursion -o BadLeftRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleLeftRecursion.yrd ^
           -g "GLLGenerator -pos int -token int -module GLL.SimpleLeftRecursion -o SimpleLeftRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleRightRecursion.yrd ^
           -g "GLLGenerator -pos int -token int -module GLL.SimpleRightRecursion -o SimpleRightRecursion.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i SimpleAmb.yrd ^
           -g "GLLGenerator -pos int -token int -module GLL.SimpleAmb -o SimpleAmb.yrd.fs" >> log.txt

..\packages\YaccConstructor.0.0.8.11\tools\YaccConstructor\YaccConstructor.exe -i Mixed.yrd ^
           -g "GLLGenerator -pos int -token int -module GLL.Mixed -o Mixed.yrd.fs" >> log.txt

