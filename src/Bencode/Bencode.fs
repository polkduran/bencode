module Bencode

type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string, BenElement>

let toString chars = System.String(chars |> Array.ofSeq)

let (|BenInt|_|) chars = 
    let toBenInt = List.rev >> toString >> int64 >> Int
    let rec parseInt acc = function
        | i::'e'::tail -> Some(toBenInt (i::acc),tail)
        | v::tail -> parseInt (v::acc) tail
        | _ -> None
    chars 
        |> function 'i'::r -> Some(r) |_->None
        |> Option.bind (parseInt [])
        
let (|BenString|_|) chars =
    let toBenString = List.rev >> toString >> String
    let rec parseString acc n = function
        | s::tail when n > 0 -> parseString  (s::acc) (n-1) tail
        | _ when n > 0 -> None
        | rest -> Some(toBenString acc, rest)
    chars
        |> function n::':'::r -> Some(n.ToString() |> int,r) |_->None
        |> Option.bind (fun (n,r) -> parseString [] n r)

let rec (|BenList|_|) chars =
    let toBenList = List.rev >> List
    let rec parseList acc = function
        |BenEl(x,tail) -> parseList (x::acc) tail
        |'e'::tail -> Some(toBenList acc, tail)
        |_ -> None
    chars
        |> function 'l'::r -> Some(r) |_-> None
        |> Option.bind (parseList [])

and (|BenEl|_|) = function
    | BenInt(x,tail) 
    | BenString(x,tail)
    | BenList(x,tail) -> Some(x,tail)
    | _ -> None


let f chars = 
    match chars with
    |BenEl(x,tail) -> (Some(x),tail)
    |c -> (None,c)

let chars = "l4:spami456elee" |> List.ofSeq
let res = f chars

