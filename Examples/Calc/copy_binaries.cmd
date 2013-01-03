rem Just for CI build.
if  exist yc (
	rd /s /q yc )

mkdir yc

xcopy /Y ..\..\YaccConstructor\Main\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\Main\bin\Release\*.exe yc
xcopy /Y ..\..\YaccConstructor\CYK\bin\Release\*.exe yc
xcopy /Y ..\..\YaccConstructor\CYK\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRCommon\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRParser\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRGenerator\bin\Release\*.dll yc

