#light

module Tree

type tree = 
     | Node of (tree list)*string*(string list)
     | Leaf of string*(string list)
     | Label