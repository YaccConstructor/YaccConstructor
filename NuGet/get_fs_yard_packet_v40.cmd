if exist FsYardPacet ( rd /s /q FsYardPacket )

mkdir FsYardPacket\content\FsYARD
mkdir FsYardPacket\lib

copy ..\Bin\Release\v40\Common.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Common.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Common.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\Constraints.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Constraints.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Constraints.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\Conversions.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Conversions.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\Conversions.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\FsYacc.exe FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FsYacc.pdb FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\RNGLR.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLR.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLR.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\RNGLRCommon.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLRCommon.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLRCommon.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\RNGLRParser.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLRParser.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLRParser.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\RNGLR.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLR.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\RNGLR.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\YardFrontend.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\YardFrontend.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\YardFrontend.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\FsYARD.exe FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FsYARD.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FsYard.targets FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FsYARD.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\FsYacc.exe FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\FSharpx.Text.StructuredFormat.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FSharpx.Text.StructuredFormat.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FSharpx.Text.StructuredFormat.xml FsYardPacket\content\FsYARD

copy ..\Bin\Release\v40\FSharpx.Core.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FSharpx.Core.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v40\FSharpx.Core.xml FsYardPacket\content\FsYARD

copy ..\YaccConstructor\FsYARD\README FsYardPacket\
copy ..\LICENSE FsYardPacket\
copy ..\NOTICE FsYardPacket\


copy ..\Bin\Release\v40\FSharpx.Core.dll FsYardPacket\lib
copy ..\Bin\Release\v40\FSharpx.Core.pdb FsYardPacket\lib
copy ..\Bin\Release\v40\FSharpx.Core.xml FsYardPacket\lib

copy ..\Bin\Release\v40\RNGLRCommon.dll FsYardPacket\lib
copy ..\Bin\Release\v40\RNGLRCommon.pdb FsYardPacket\lib
copy ..\Bin\Release\v40\RNGLRCommon.XML FsYardPacket\lib

copy ..\Bin\Release\v40\RNGLRParser.dll FsYardPacket\lib
copy ..\Bin\Release\v40\RNGLRParser.pdb FsYardPacket\lib
copy ..\Bin\Release\v40\RNGLRParser.XML FsYardPacket\lib

copy ..\YaccConstructor\FsYARD\FsYARD.nuspec FsYardPacket\
nuget pack FsYardPacket\FsYARD.nuspec

