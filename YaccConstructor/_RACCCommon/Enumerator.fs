// Enumerator.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

namespace Yard.Generators._RACCGenerator

type Enumerator() = class
    let i = ref -1
    let next() = incr i; !i          
    member self.Next() = next()
    member self.Reset() = i := -1   
end

