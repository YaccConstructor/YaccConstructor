
[ -f log.txt ] && rm log.txt
                                                                                               
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i InfEpsilon.yrd -g "GLLGenerator -pos int -token int -module GLL.ParseInfEpsilon -o InfEpsilon.yrd.fs" >> log.txt