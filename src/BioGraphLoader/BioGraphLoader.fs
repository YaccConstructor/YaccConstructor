module YC.BIO.BioGraphLoader

open System.IO

let lodGraph fileWithoutExt =
    let lblsExt = ".sqn"
    let graphStrauctureExt = ".grp"
    let lbls = 
        File.ReadAllLines(fileWithoutExt + lblsExt) 
        |> 

