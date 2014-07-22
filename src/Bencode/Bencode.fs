module Bencode
open System

type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string,BenElement>    

let toString chars = System.String(chars |> Array.ofSeq)

let (|Num|_|) (c:char) = 
    let intVal = int c
    if intVal > 47 && intVal < 58 then Some(c) else None

let toInt = List.rev >> toString >> int64

let (|Nums|_|) suffix chars = 
    let rec parseInternal acc = function
            |Num(c)::s::tail when s = suffix -> Some(toInt (c::acc), tail)
            |Num(c)::tail -> parseInternal (c::acc) tail
            |_ -> None
    parseInternal [] chars 

let (|BenInt|_|) = function
    |'i'::Nums 'e' (i, tail) -> Some(Int(i),tail)
    |_ -> None
        
let (|BenString|_|) chars =
    let toBenString = List.rev >> toString >> String
    let rec parseString acc = function
        | n,s::tail when n > 0L -> parseString  (s::acc) (n-1L, tail)
        | n,_ when n > 0L -> None
        | _,rest -> Some(toBenString acc, rest)
    chars 
        |> function Nums ':' (i,tail) -> Some(i,tail)|_->None
        |> Option.bind (parseString [])
 
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
    | BenList(x,tail) 
    | BenDic(x, tail)
    | BenString(x,tail) -> Some(x,tail)
    | _ -> None

and (|BenDic|_|) chars =
    let buildMap kvs = Dict( new Map<string,BenElement>(kvs) )
    let rec parseKv acc = function
        | BenString(String(k), BenEl(v,tail)) -> parseKv ((k,v)::acc) tail
        | 'e'::tail -> Some(buildMap acc, tail)
        | _ -> None
    chars
        |> function 'd'::r -> Some(r) |_-> None
        |> Option.bind (parseKv [])


