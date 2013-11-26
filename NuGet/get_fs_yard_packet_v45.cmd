if exist FsYardPacet ( rd /s /q FsYardPacket )

mkdir FsYardPacket\content\FsYARD
mkdir FsYardPacket\lib

copy ..\Bin\Release\v45\Common.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Common.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Common.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\Constraints.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Constraints.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Constraints.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\Conversions.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Conversions.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\Conversions.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\FsYacc.exe FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FsYacc.pdb FsYardPacket\content\FsYARD

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
copy ..\Bin\Release\v45\FsYard.targets FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FsYARD.XML FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\FsYacc.exe FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\FSharpx.Text.StructuredFormat.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FSharpx.Text.StructuredFormat.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FSharpx.Text.StructuredFormat.xml FsYardPacket\content\FsYARD

copy ..\Bin\Release\v45\FSharpx.Core.dll FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FSharpx.Core.pdb FsYardPacket\content\FsYARD
copy ..\Bin\Release\v45\FSharpx.Core.xml FsYardPacket\content\FsYARD

copy ..\YaccConstructor\FsYARD\README FsYardPacket\
copy ..\LICENSE FsYardPacket\
copy ..\NOTICE FsYardPacket\


copy ..\Bin\Release\v45\FSharpx.Core.dll FsYardPacket\lib
copy ..\Bin\Release\v45\FSharpx.Core.pdb FsYardPacket\lib
copy ..\Bin\Release\v45\FSharpx.Core.xml FsYardPacket\lib

copy ..\Bin\Release\v45\RNGLRCommon.dll FsYardPacket\lib
copy ..\Bin\Release\v45\RNGLRCommon.pdb FsYardPacket\lib
copy ..\Bin\Release\v45\RNGLRCommon.XML FsYardPacket\lib

copy ..\Bin\Release\v45\RNGLRParser.dll FsYardPacket\lib
copy ..\Bin\Release\v45\RNGLRParser.pdb FsYardPacket\lib
copy ..\Bin\Release\v45\RNGLRParser.XML FsYardPacket\lib

copy ..\YaccConstructor\FsYARD\FsYARD.nuspec FsYardPacket\
nuget pack FsYardPacket\FsYARD.nuspec

