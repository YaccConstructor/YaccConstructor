namespace Yard.Generators.RecursiveAscent

type ErrorHandler() = class    
    let handle error = 
        printfn "\n Error is detected in position %A \n" (error.ePosition)
    
    member self.Handle error = handle error
end

