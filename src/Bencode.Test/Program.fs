open System
[<EntryPoint>]
let main argv = 
    let f chars = 
        match chars with
        |Bencode.BenEl(x,tail) -> (Some(x),tail)
        |c -> (None,c)

    let mutable str = "l9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee"
    str<-"17:publisher-webpage"
    //str<-"d9:publisherle4:spami456ee"
    let chars = str |> List.ofSeq

    let res = f chars

    printfn "%A" res

    Console.ReadLine() |> ignore

    0 // return an integer exit code
