#I @"..\..\Bin\Release\v40"

#r @"YC.Common.dll"
#r @"YC.GLLGenerator.dll"
#r @"YC.YardFrontend.dll"

open Yard.Generators.GLL

module BioSearch =
    let fe = new YardFrontend()
