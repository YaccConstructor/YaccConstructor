module Solution
    open System.IO
    #load "project.fsx"
    
    let projects = ref []

    let addPrj p = projects := p :: !projects

    let Common = 
        let p = new Projects.Project()
        p.Name <- "Common.fsproj"
        p.Folder <- @"./Core/Common/"
        p.SvnSrcFolder <- @"../YaccConstructor/Common/"
        addPrj p
        p

    let Conversions = 
        let p = new Projects.Project()
        p.Name <- "Conversions.fsproj"
        p.Folder <- @"./Core/Conversions/"
        p.SvnSrcFolder <- @"../YaccConstructor/Conversions/"
        addPrj p
        p

    let Main = 
        let p = new Projects.Project()
        p.Name <- "Main.fsproj"
        p.Folder <- @"./Core/Main/"
        p.SvnSrcFolder <- @"../YaccConstructor/Main/"
        addPrj p
        p
    
    let Yard = 
        let p = new Projects.Project()
        p.Name <- "YardFrontend.fsproj"
        p.Folder <- @"./Frontends/Yard/"
        p.SvnSrcFolder <- @"../YaccConstructor/YardFrontend/"
        addPrj p
        p

    let RACCCommon = 
        let p = new Projects.Project()
        p.Name <- "RACCCommon.fsproj"
        p.Folder <- @"./Generators/RACC/Common/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCCommon/"
        addPrj p
        p

    let RACCCore = 
        let p = new Projects.Project()
        p.Name <- "RACCCore.fsproj"
        p.Folder <- @"./Generators/RACC/Core/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCCore/"
        addPrj p
        p

    let RACCFA = 
        let p = new Projects.Project()
        p.Name <- "RACCFiniteAutomata.fsproj"
        p.Folder <- @"./Generators/RACC/FA/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCFiniteAutomata/"
        addPrj p
        p

    let RACCGenerator = 
        let p = new Projects.Project()
        p.Name <- "RACCGenerator.fsproj"
        p.Folder <- @"./Generators/RACC/Generator/"
        p.SvnSrcFolder <- @"../YaccConstructor/RACCGenerator/"
        addPrj p
        p

    let SetMode mode =
        List.iter (fun (p:Projects.Project) -> p.Mode <- mode) (!projects)

    let SetPlatform platform =
        List.iter (fun (p:Projects.Project) -> p.Platform <- platform) (!projects) 

    let SetNetVer netVer =
        List.iter (fun (p:Projects.Project) -> p.NetVer <- netVer) (!projects)

    let SetOptimize optimize =
        List.iter (fun (p:Projects.Project) -> p.Optimize <- optimize)(!projects) 
