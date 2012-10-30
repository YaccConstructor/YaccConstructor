if  exist yc (
	rd /s /q yc )

mkdir yc

xcopy /Y ..\YaccConstructor\Main\bin\Debug\*.dll yc
xcopy /Y ..\YaccConstructor\Main\bin\Debug\*.exe yc
xcopy /Y ..\YaccConstructor\CYK\bin\Debug\*.exe yc
xcopy /Y ..\YaccConstructor\CYK\bin\Debug\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRCommon\bin\Debug\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRParser\bin\Debug\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRGenerator\bin\Debug\*.dll yc

