if exist FsYardPacet ( rd /s /q FsYardPacket )

mkdir FsYardPacket\content\FsYARD
mkdir FsYardPacket\lib
mkdir FsYardPacket\lib\45
mkdir FsYardPacket\lib\40

copy ..\Bin\Release\v45\Common.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Common.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Common.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\Constraints.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Constraints.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Constraints.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\Conversions.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Conversions.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Conversions.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Tools\Release\bin\FsYacc.exe FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Tools\Release\bin\FsYacc.pdb FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\RNGLR.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLR.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLR.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\RNGLRCommon.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLRCommon.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLRCommon.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\RNGLRParser.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLRParser.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLRParser.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\RNGLR.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLR.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\RNGLR.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\YardFrontend.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\YardFrontend.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\YardFrontend.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\FsYARD.exe FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FsYARD.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\FsYARD\FsYard.targets FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FsYARD.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Tools\FsYacc.exe FsYardPacket\content\FsYARD

copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.xml FsYardPacket\content\FsYARD

copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.xml FsYardPacket\content\FsYARD

copy ..\YaccConstructor\FsYARD\README FsYardPacket\
copy ..\LICENSE FsYardPacket\
copy ..\NOTICE FsYardPacket\


copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll FsYardPacket\lib\45\
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.pdb FsYardPacket\lib\45\
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.xml FsYardPacket\lib\45\

copy ..\Bin\Release\v45\RNGLRCommon.dll FsYardPacket\lib\45\
copy ..\Bin\Release\v45\RNGLRCommon.pdb FsYardPacket\lib\45\
copy ..\Bin\Release\v45\RNGLRCommon.XML FsYardPacket\lib\45\

copy ..\Bin\Release\v45\RNGLRParser.dll FsYardPacket\lib\45\
copy ..\Bin\Release\v45\RNGLRParser.pdb FsYardPacket\lib\45\
copy ..\Bin\Release\v45\RNGLRParser.XML FsYardPacket\lib\45\

copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll FsYardPacket\lib\40\
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.pdb FsYardPacket\lib\40\
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.xml FsYardPacket\lib\40\

copy ..\Bin\Release\v40\RNGLRCommon.dll FsYardPacket\lib\40\
copy ..\Bin\Release\v40\RNGLRCommon.pdb FsYardPacket\lib\40\
copy ..\Bin\Release\v40\RNGLRCommon.XML FsYardPacket\lib\40\

copy ..\Bin\Release\v40\RNGLRParser.dll FsYardPacket\lib\40
copy ..\Bin\Release\v40\RNGLRParser.pdb FsYardPacket\lib\40
copy ..\Bin\Release\v40\RNGLRParser.XML FsYardPacket\lib\40


copy ..\YaccConstructor\FsYARD\FsYARD.nuspec FsYardPacket\
nuget pack FsYardPacket\FsYARD.nuspec

