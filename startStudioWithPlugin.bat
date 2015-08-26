SET pathToVS12=C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\devenv.exe
SET pathToVS11=C:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE\devenv.exe

IF EXIST %pathToVS12%(
"%pathToVS12%" /RootSuffix YC /ReSharper.Internal
Exit /b
)

IF EXIST %pathToVS11%
(
"%pathToVS11%" /RootSuffix YC /ReSharper.Internal
Exit /b
)
ELSE
(
echo "Microsoft Visual Studio 11 and Microsoft Visual Studio 12 weren't found. Try run plugin manually with keys /RootSuffix YC /ReSharper.Internal"
)