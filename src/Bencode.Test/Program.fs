open System
[<EntryPoint>]
let main argv = 

    let mutable str = "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homeeee"
    //str<-"17:publisher-webpage"
    //str<-"d9:publisherle4:spami456ee"
    let chars = str |> List.ofSeq

    let res = Bencode.BenDecode.parseChars chars

    printfn "%A" res

    Console.ReadLine() |> ignore

    0 // return an integer exit code
