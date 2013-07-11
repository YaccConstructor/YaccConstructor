@echo off
..\YaccConstructor\bin\Release\YaccConstructor.exe  -g FParsecGenerator  -i ..\..\Tests\FParsec\calc.yrd > log.txt
..\YaccConstructor\bin\Release\YaccConstructor.exe  -g FParsecGenerator  -i ..\..\Tests\FParsec\literals.yrd > log1.txt
