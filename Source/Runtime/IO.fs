// IO.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

module Yard.Core.IO

open System.IO
open IL
                               
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


let readValue path =
    use inStream = new FileStream(path, FileMode.Open)     
    let deserializer = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()        
    let data = deserializer.Deserialize(inStream)
    unbox(data)