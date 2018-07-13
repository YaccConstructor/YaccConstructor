module FsLexGenerator

open System.IO
open System

let genBrackets template count =
        [|0..count - 1|] 
        |> Array.map (fun i -> sprintf template i i)
        |> String.concat " | " 

let generateFsLexSource asserts calls locks =
    use writer = File.CreateText "lex.fsl"
    
    let writeln (line: string) = writer.WriteLine line
    
    writeln "{}"
    
    writeln ""
    let assertsAlts = 
        Seq.map (fun i -> sprintf "\"A%i\"" i) (seq {0..asserts})
        |> String.concat " | "
    
    writeln ("let ba = (" + assertsAlts + ")")
    writeln ("let ca = (" + assertsAlts + ")")
    
    let genBrs tmplt count =
        [|0..count - 1|] 
        |> Array.map (fun i -> sprintf tmplt i i)
        |> String.concat " | " 
        
    let s0Head  = "rule s0 = parse ca \"s0\" {} | ca {} "
    let s0Calls = genBrs " \"C%i\" \"s0\" \"RT%i\" \"s0\" {}" calls
    let s0Locks = genBrs " \"G%i\" \"s0\" \"RL%i\" \"s0\" {}" locks
    
    writeln (s0Head + " | " + s0Calls + " | " + s0Locks)
    
    let s1Head  = "and s1 = parse "
    let s1Calls = genBrs " \"C%i\" \"s1\" \"RT%i\" \"s1\" {}" calls
    let s1Locks = genBrs " \"G%i\" \"s0\" \"RL%i\" \"s1\" {}" locks
    
    writeln (s1Head + s1Calls + " | " + s1Locks)
    
    let sHead = "and s = parse ba \"s\" {} | \"s\" ba {} | \"s\" \"s1\" {} | \"s1\" \"s\" {} | ba {}"
    let sCalls1 = genBrs " \"C%i\" \"s\" \"RT%i\" \"s1\" {}" calls
    let sCalls3 = genBrs " \"C%i\" \"s\" \"RT%i\" \"s\" {}" calls
    
    writeln (sHead + " | " + sCalls1 + " | " + sCalls3)
    
    
