open System
[<EntryPoint>]
let main argv = 
    let f chars = 
        match chars with
        |Bencode.BenEl(x,tail) -> (Some(x),tail)
        |c -> (None,c)

//    let nums = ["0e";"1e";"32e";"-256e";"-012e";"-0e";"45ie"]
//    let r = nums 
//            |> List.map List.ofSeq
//            |> List.map (Bencode.tryGetNums 'e')
//
//    
//    printfn "%A" r

    let mutable str = "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee"
    //str<-"17:publisher-webpage"
    //str<-"d9:publisherle4:spami456ee"
    let chars = str |> List.ofSeq

    let res = f chars

    printfn "%A" res

    Console.ReadLine() |> ignore

    0 // return an integer exit code
