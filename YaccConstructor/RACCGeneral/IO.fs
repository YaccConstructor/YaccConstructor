// IO.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module Yard.Generators.RecursiveAscent.IO

open System.IO
open Yard.Core.IL
open System.Xml
open System.Reflection 
open System.Runtime.Serialization
                               
let writeValue out_path value = 

    use out_stream = new FileStream(out_path, FileMode.Create)
    use writer =  new BinaryWriter(out_stream); 
    let serializer = new System.
                       Runtime.
                         Serialization.
                           Formatters.
                             Binary.
                               BinaryFormatter()
    serializer.Serialize(out_stream, box value);
    writer.Close();
    out_stream.Close();
    
    
let binary_reader path =     
    try
       let inStream = new FileStream(path, FileMode.OpenOrCreate)       
       new BinaryReader (inStream)      
    with e -> printf "%A " e.ToString; failwith "reader_exception"

let text_writer path = 
    try
       let t = new FileInfo(path);
       let writer = t.CreateText();              
       writer     
    with e -> printf "%A " e.ToString; failwith "writer_exception"

let toString (v:obj) =
    match v with
    | :? string -> "\"" + v.ToString() + "\""
    | _         -> v.ToString()

let printSet _set printItem = "set [|" + String.concat ";" (Set.map printItem _set) + "|]"
let printList lst printItem = "(List.ofArray [|" + String.concat ";" (List.map printItem lst) + "|])"
let printDict lst printItem = "dict <| " + printList lst printItem 

let writeTables path goto items nTerms ruleToActionMap = 
    let dataForWrite = 
          "module Tables \n\n"
        + "open Yard.Generators.RecursiveAscent.Grammar.Item \n"
        + "open Yard.Generators.RecursiveAscent\n\n"
        + "let items =\n   "
        + printSet (items) (fun x -> x.ToString())
        + "\n\n"
        + "let gotoSet =\n   "
        + printDict goto (fun (x,y) -> "(" + x.ToString() + "," + printSet y (fun y -> y.ToString()) + ")")
        + "\n\n"
        + "let startNterms =\n    "
        + printList nTerms toString
        + "\n\n"
        + "let ruleToActionMap =\n    "
        + printDict ruleToActionMap (fun (x,y) -> "("+ toString x + "," + toString y + ")")
        
    let outStream = text_writer(path + ".tables.fs")
    outStream.Write(dataForWrite)
    outStream.Close()
    
let readValue path =
    use inStream = new FileStream(path, FileMode.Open)         
    let deserializer = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()       
    let data = deserializer.Deserialize(inStream)
    unbox(data)