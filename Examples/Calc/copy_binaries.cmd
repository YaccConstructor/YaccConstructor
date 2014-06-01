rem Just for CI build.
if  exist yc (
    rd /s /q yc )

mkdir yc

xcopy /Y ..\..\YaccConstructor\YaccConstructor\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\YaccConstructor\bin\Release\*.exe yc
xcopy /Y ..\..\YaccConstructor\CYK\bin\Release\*.exe yc
xcopy /Y ..\..\YaccConstructor\CYK\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRCommon\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\GLLCommon\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRParser\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRAbstractParser\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\RNGLRGenerator\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\GLLParser\bin\Release\*.dll yc          
xcopy /Y ..\..\YaccConstructor\GLLGenerator\bin\Release\*.dll yc
xcopy /Y ..\..\YaccConstructor\Constraints\bin\Release\*.dll yc
