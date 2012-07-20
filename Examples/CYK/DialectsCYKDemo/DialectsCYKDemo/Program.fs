let run inStr =
    let  buf = Lexing.LexBuffer<_>.FromString inStr 
    let tokens =
        seq { 
            while not buf.IsPastEndOfStream do
            let t = Lexer.token buf
            yield  t
        }
        |> Yard.Generators.CYK.CodeTokenStream
        |> Seq.filter ((<>) -1)
        |> Seq.map (fun x -> uint16 x)
        |> Array.ofSeq
    let cyk = new Yard.Generators.CYKGenerator.CYKCore()
    cyk.Recognize (new ResizeArray<_>(Yard.Generators.CYK.rules),Yard.Generators.CYK.StartNTerm) tokens (fun x y z -> 0uy)

do printfn "%s" <| run "9+"