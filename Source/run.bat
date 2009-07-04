@echo off
echo get new Generator.dll
xcopy /y /s Generator\bin\Release\Generator.dll Runner\bin\Release\Generator.dll
echo Generating...
Runner\bin\Release\Runner.exe
echo Copy data files...
xcopy /y  Runner\bin\Release\goto.dta  Pasrser\bin\Release\
xcopy /y  Runner\bin\Release\items.dta  Pasrser\bin\Release\
echo Parsing...
Pasrser\bin\Release\pasrser.exe