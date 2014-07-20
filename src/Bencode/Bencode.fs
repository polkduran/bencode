module Bencode
open System

type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string,BenElement>    

let toString chars = System.String(chars |> Array.ofSeq)

let (|Num|_|) (c:char) = match Int32.TryParse(c.ToString()) with
                            | true,_ -> Some(c)
                            | _ -> None

let toInt = List.rev >> toString >> int64


let (|Nums|_|) suffix chars =
    let rec parseInternal acc = function
        |Num(c)::tail -> parseInternal (c::acc) tail
        |suffix::tail -> Some(toInt acc, tail)
        |_ -> None
    parseInternal [] chars

let (|BenInt|_|) chars = 
    let toBenInt = toInt >> Int
    let rec parseInt acc = function
        | i::'e'::tail -> Some(toBenInt (i::acc),tail)
        | v::tail -> parseInt (v::acc) tail
        | _ -> None
    chars 
        |> function 'i'::r -> Some(r) |_->None
        |> Option.bind (function Nums(i,tail) 'e' -> Some(Int(i),tail)|_-> None )
        
let (|BenString|_|) chars =
    let toBenString = List.rev >> toString >> String
    let rec strLen acc = function
            | Num(i)::r -> strLen (i::acc) r
            | ':'::r -> Some(toInt acc)
            |_ -> None 
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
    | BenList(x,tail) 
    | BenDic(x, tail) -> Some(x,tail)
    | _ -> None

and (|BenDic|_|) chars =
    let buildMap kvs = Dict( new Map<string,BenElement>(kvs) )
    let rec parseKv acc = function
        | BenString(String(k), BenEl(v,tail)) 
                -> parseKv ((k,v)::acc) tail
//        | BenString(String(k), r) ->
//            match r with | BenEl(x,tail) -> parseKv ((k,x)::acc) tail
//                         |_ -> None 
        | 'e'::tail -> Some(buildMap acc, tail)
        | _ -> None
    chars
        |> function 'd'::r -> Some(r) |_-> None
        |> Option.bind (parseKv [])

let f chars = 
    match chars with
    |BenEl(x,tail) -> (Some(x),tail)
    |c -> (None,c)

let mutable str = "l9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee"
str<-"17:publisher-webpage"
//str<-"d9:publisherle4:spami456ee"
let chars = str |> List.ofSeq

let res = f chars

