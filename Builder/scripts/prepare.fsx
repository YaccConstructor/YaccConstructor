#I "BuildHelpers/BuildHelpers/bin/Release"
#r "BuildHelpers.exe"

let dependencesForRemove =
    [ ("YaccConstructor/YaccConstructor.fsproj"
           ,[ @"..\AntlrFrontend\AntlrFrontend.fsproj"
             ;@"..\CYKGenerator\CYKGenerator.fsproj"
             ;@"..\FParsecGenerator\FParsecGenerator.fsproj"
             ;@"..\FsYaccFrontend\FsYaccFrontend.fsproj"
             ;@"..\FsYaccPrinter\FsYaccPrinter.fsproj"
             ;@"..\RNGLRGenerator\RNGLRGenerator.fsproj"
             ;@"..\TreeDump\TreeDump.fsproj"
             ;@"..\YardFrontend\YardFrontend.fsproj"
             ;@"..\YardPrinter\YardPrinter.fsproj"
             ;@"Irony"
             ;@"Irony.Samples"])
    ]

let basePath = @".\..\workdir\src"

do dependencesForRemove |> List.iter (fun (pr,deps) -> System.IO.Path.Combine(basePath,pr) |> YaccConstructor.Build.Helpers.JustRmDependences deps)