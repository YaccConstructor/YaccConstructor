..\tools\Build.Tools\Fake\FAKE.exe ..\tools\Build.Tools\Core.fsx "tools=..\tools\Build.Tools" "target=Packaging:Restore" "packages=packages" "solution=YaccConstructor.sln"

cd .\YardFrontend
call gen.cmd %1
@echo on
cd ..\

..\tools\Build.Tools\Fake\FAKE.exe ..\tools\Build.Tools\Core.fsx "tools=..\tools\Build.Tools" "target=Solution:Build" "solution=YaccConstructor.WithoutTests.sln"

cd .\RNGLRParser.SimpleTest
call gen.cmd  %1
@echo on
cd ..\

cd .\RNGLRParser.ErrorRecoveryTest
call gen.cmd  %1
@echo on
cd ..\

cd .\RNGLRAbstractParser.Test
call gen.cmd %1
@echo on
cd ..\

cd .\RNGLRAbstractParser.Test
call gen_lex.cmd %1
@echo on
cd ..\

..\tools\Build.Tools\Fake\FAKE.exe ..\tools\Build.Tools\Core.fsx "tools=..\tools\Build.Tools" "target=Solution:Build" "solution=YaccConstructor.sln"