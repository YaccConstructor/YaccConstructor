let x y = 1,2,y

let a,b,c = x 3

[<Struct>]
type _t1 =
    val _1 : int
    val _2 : int
    val _3 : int

_t1 x(int y) =
    return new _t1()
let _r1 = x(3)
let a = _r._1
let b = _r._2
let c = _r._3
(*

структуры
массивы сложных типов

*)
