module Bencode

type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string, BenElement>

let toString chars = System.String(chars |> Array.ofSeq)

let chars = "i456789e" |> List.ofSeq
let rec iss acc = function
                    | v::['e'] -> Some(v::acc)
                    | v::r -> iss (v::acc) r   
                    | _ -> None  
let i = chars 
            |> function 'i'::r -> Some(r) | _ -> None
            |> Option.bind (iss [] )

let (|BenInt|_|) = 
    let rec loop acc = function
        | 'i'::rest -> loop [] rest
        | 'e'::[] -> Some(acc |> List.rev |> toString |> int64)
        | c::rest ->  loop (c::acc) rest
        | _ -> None
    loop [] 

let (|BenString|_|) =
    let rec loop acc = function
        |


let (BenInt i) = chars

let parse input =
