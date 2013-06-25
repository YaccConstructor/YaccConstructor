echo off
echo "##teamcity[testStarted name='Generate MS-SQL grammar.' captureStandardOutput='true']"

..\..\yc\YaccConstructor.exe -g "GLLGenerator -token SourceText -module Yard.Examples.MSParser -o mssql.yrd.fs" -i ..\..\RNGLR\MSSql\mssql.yrd > log.txt

set el=%errorlevel%
echo %el%
if "%el%"=="0" (
     echo "##teamcity[testFinished name='Generate MS-SQL grammar.']"
) else (
     echo "##teamcity[testFailed name='Generate MS-SQL grammar.']"
)
exit %el%