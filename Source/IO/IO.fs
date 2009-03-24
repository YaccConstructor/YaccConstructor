#light

module IO

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

let readValue path =
    use inStream = new FileStream(path, FileMode.Open)
    use reader = new BinaryReader(inStream)                
    let deserializer = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    unbox(deserializer.Deserialize(inStream))