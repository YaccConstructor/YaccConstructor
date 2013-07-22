if  exist yc (
    rd /s /q yc )

mkdir yc

xcopy /Y ..\YaccConstructor\YaccConstructor\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\YaccConstructor\bin\Release\*.exe yc
xcopy /Y ..\YaccConstructor\CYK\bin\Release\*.exe yc
xcopy /Y ..\YaccConstructor\GLLGenerator\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\CYK\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRCommon\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRParser\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRGenerator\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\Constraints\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\Utils.SourceText\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\FsYard\bin\Release\*.exe yc
xcopy /Y ..\YaccConstructor\FsYard\bin\Release\*.targets yc
xcopy /Y ..\YaccConstructor\AbstractLexer.Common\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\AbstractLexer.Core\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\AbstractParsing.Common\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\RNGLRAbstractParser\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\AbstractLexer.Generator\bin\Release\*.exe yc




