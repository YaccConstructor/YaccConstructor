@echo off

del log.txt

..\YaccConstructor\bin\Release\YaccConstructor.exe -i Calc.yrd -g "RNGLRGenerator" >> log.txt
