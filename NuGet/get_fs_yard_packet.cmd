if exist FsYardPacet ( rd /s /q FsYardPacket )

mkdir FsYardPacket\content\FsYARD
mkdir FsYardPacket\lib

copy ..\YaccConstructor\Common\bin\Release\Common.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Common\bin\Release\Common.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Common\bin\Release\Common.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Constraints\bin\Release\Constraints.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Constraints\bin\Release\Constraints.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Constraints\bin\Release\Constraints.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Conversions\bin\Release\Conversions.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Conversions\bin\Release\Conversions.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Conversions\bin\Release\Conversions.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Tools\Release\bin\FsYacc.exe FsYardPacket\content\FsYARD
copy ..\YaccConstructor\Tools\Release\bin\FsYacc.pdb FsYardPacket\content\FsYARD

copy ..\YaccConstructor\RNGLRGenerator\bin\Release\RNGLR.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\RNGLRGenerator\bin\Release\RNGLR.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\RNGLRGenerator\bin\Release\RNGLR.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\YardFrontend\bin\Release\YardFrontend.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\YardFrontend\bin\Release\YardFrontend.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\YardFrontend\bin\Release\YardFrontend.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\FsYARD\bin\Release\FsYARD.exe FsYardPacket\content\FsYARD
copy ..\YaccConstructor\FsYARD\bin\Release\FsYARD.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\FsYARD\FsYard.targets FsYardPacket\content\FsYARD
copy ..\YaccConstructor\FsYARD\bin\Release\FsYARD.XML FsYardPacket\content\FsYARD

copy ..\YaccConstructor\Tools\FsYacc.exe FsYardPacket\content\FsYARD

copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.dll FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.pdb FsYardPacket\content\FsYARD
copy ..\YaccConstructor\packages\FSharpx.Text.StructuredFormat.1.8.41\lib\40\FSharpx.Text.StructuredFormat.xml FsYardPacket\content\FsYARD

copy ..\YaccConstructor\FsYARD\README FsYardPacket\
copy ..\LICENSE FsYardPacket\
copy ..\NOTICE FsYardPacket\


copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll FsYardPacket\lib
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.pdb FsYardPacket\lib
copy ..\YaccConstructor\packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.xml FsYardPacket\lib

copy ..\YaccConstructor\RNGLRCommon\bin\Release\RNGLRCommon.dll FsYardPacket\lib
copy ..\YaccConstructor\RNGLRCommon\bin\Release\RNGLRCommon.pdb FsYardPacket\lib
copy ..\YaccConstructor\RNGLRCommon\bin\Release\RNGLRCommon.XML FsYardPacket\lib

copy ..\YaccConstructor\RNGLRParser\bin\Release\RNGLRParser.dll FsYardPacket\lib
copy ..\YaccConstructor\RNGLRParser\bin\Release\RNGLRParser.pdb FsYardPacket\lib
copy ..\YaccConstructor\RNGLRParser\bin\Release\RNGLRParser.XML FsYardPacket\lib

copy ..\YaccConstructor\FsYARD\FsYARD.nuspec FsYardPacket\
nuget pack FsYardPacket\FsYARD.nuspec

