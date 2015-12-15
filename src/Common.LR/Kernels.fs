module Yard.Generators.Common.LR.Kernels

type OutTable = LR | LALR

type Kernel = int
//type Item = Kernel * Set<int>

type KernelInterpreter =
    static member inline toKernel (prod,pos) = (prod <<< 16) ||| pos
    static member inline incPos kernel = kernel + 1
    static member inline getProd kernel = kernel >>> 16
    static member inline getPos kernel = kernel &&& ((1 <<< 16) - 1)
    static member inline unzip kernel = (KernelInterpreter.getProd kernel, KernelInterpreter.getPos kernel)
    static member inline kernelsOfState = fst
    static member inline lookAheadsOfState = snd

type RnglrReduceLabel =
    | Reduce
    | ZeroReduce