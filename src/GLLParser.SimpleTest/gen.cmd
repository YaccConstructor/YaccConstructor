
del log.txt
                                                                                               
..\..\Bin\Release\v40\YC.YaccConstructor.exe  -i BadLeftRecursion.yrd ^
           -g "GLLGenerator -pos int -token int -abstract false -module GLL.BadLeftRecursion -o BadLeftRecursion.yrd.fs" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleLeftRecursion.yrd ^
           -g "GLLGenerator -pos int -token int  -abstract false  -module GLL.SimpleLeftRecursion -o SimpleLeftRecursion.yrd.fs" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightRecursion.yrd ^
           -g "GLLGenerator -pos int -token int  -abstract false -module GLL.SimpleRightRecursion -o SimpleRightRecursion.yrd.fs" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd ^
           -g "GLLGenerator -pos int -token int  -abstract false -module GLL.SimpleAmb -o SimpleAmb.yrd.fs" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Mixed.yrd ^
           -g "GLLGenerator -pos int -token int  -abstract false  -module GLL.Mixed -o Mixed.yrd.fs" >> log.txt

