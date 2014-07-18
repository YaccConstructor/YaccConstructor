cd ..
tools\Build.Tools\Fake\FAKE.exe tools\Build.Tools\Core.fsx "tools=tools\Build.Tools" "target=Packaging:Restore" "packages=src\packages" "solution=src\YaccConstructor.sln"
cd BuildScripts

cd ..\src\YardFrontend
call gen.cmd
cd ..\..\BuildScripts

call YaccConstructor.WithoutTests.bat

cd ..\src\RNGLRAbstractParser.Test
call gen.cmd %1 
cd ..\..\BuildScripts

cd ..\src\RNGLRAbstractParser.Test
call gen_lex.cmd %1
cd ..\..\BuildScripts

cd ..\src\RNGLRParser.ErrorRecoveryTest
call gen.cmd %1
cd ..\..\BuildScripts

cd ..\src\RNGLRParser.SimpleTest
call gen.cmd %1
cd ..\..\BuildScripts

..\tools\Build.Tools\Fake\FAKE.exe ..\tools\Build.Tools\Core.fsx "tools=..\tools\Build.Tools" "solution=..\src\YaccConstructor.sln"