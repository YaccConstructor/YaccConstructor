module ReshrperTreeUtils

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree

let getStringTypedParams (methodDecl: IMethodDeclaration) =
    methodDecl.Params.ParameterDeclarations
    |> Seq.choose 
        (fun p -> if p.Type.IsString() then Some(p.DeclaredName) else None)