module InnerTypeImpl
    open MatrixKernels

    type InnerFloatHandler =       
        interface IInnerTypeHandler<float> with
            member this.innerZero = 0.0
            member this.innerOne = 1.0
            member this.innerSum f1 f2 = f1 + f2
            member this.innerMult f1 f2 = f1 * f2