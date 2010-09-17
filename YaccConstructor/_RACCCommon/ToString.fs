// ToString.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

module  Yard.Generators._RACCGenerator.ToString

let rec toString (_val:obj) =
    match _val with
    | :? string  -> "\"" + (_val.ToString()) + "\""
    | :? char    -> "'" + (_val.ToString()) + "'"   
    | _           -> _val.ToString()

and listToString _list =
     "List.ofArray [|" + (String.concat ";" (List.map toString _list)) + "|]"    
and dictToString (_dict:System.Collections.Generic.IDictionary<_,_>) =
    "dict [|" + (String.concat ";" (Seq.map
                                     (fun k -> "("+ toString k + "," + (toString _dict.[k]) + ")")
                                     _dict.Keys)) + "|]" 
and setToString _set =
    "Set.ofArray [|" + (String.concat ";" (Set.map toString _set)) + "|]"    


