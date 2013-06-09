echo off
echo "##teamcity[testStarted name='Generate MS-SQL grammar.' captureStandardOutput='true']"

..\..\yc\FsYard.exe -translate false -module Yard.Examples.MSParser ^
    -infEpsPath epsilons -o MsParser.fs -i mssql.yrd -pos uint64 > log.txt

set el=%errorlevel%
echo %el%
if "%el%"=="0" (
     echo "##teamcity[testFinished name='Generate MS-SQL grammar.']"
) else (
     echo "##teamcity[testFailed name='Generate MS-SQL grammar.']"
)
exit %el%