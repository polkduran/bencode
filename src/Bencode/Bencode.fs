module Bencode
open System

type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string,BenElement>    

let private toStringRev chars = System.String(chars |> List.rev |> Array.ofSeq)
let private toInt = toStringRev >> int64

let private prefixed prefix = function 
                | head::tail when head = prefix -> Some(tail)
                | _ -> None

let private (|Num|_|) (c:char) = if c >= '0' && c <= '9' then Some(c) else None
let private (|End|_|) = function 'e'::tail -> Some(tail) |_ -> None

let private tryGetNums suffix chars =
    let rec parseInternal acc = function
            |Num(c)::s::tail when s = suffix -> Some(toInt (c::acc), tail)
            |Num(c)::tail -> parseInternal (c::acc) tail
            |_ -> None
    match chars with
        |'-'::'0'::tail -> None
        |'-'::tail -> parseInternal ['-'] tail
        |_ as tail -> parseInternal [] tail

let (|BenInt|_|) chars =
    chars 
        |> prefixed 'i'
        |> Option.bind (tryGetNums 'e')
        |> Option.map (fun (i,tail) -> Int i,tail)
        
let (|BenString|_|) chars =
    let toBenString = toStringRev >> String
    let rec parseString acc = function
        | n,s::tail when n > 0L -> parseString  (s::acc) (n-1L, tail)
        | n,_ when n > 0L -> None
        | _,rest -> Some(toBenString acc, rest)
    chars 
        |> tryGetNums ':'
        |> Option.bind (parseString [])
 
let rec (|BenList|_|) chars =
    let rec parseList acc = function
        |BenEl(x,tail) -> parseList (x::acc) tail
        |End(tail) -> Some(acc |> List.rev |> List, tail)
        |_ -> None
    chars
        |> prefixed 'l'
        |> Option.bind (parseList [])

and (|BenDic|_|) chars =
    let buildMap kvs = Dict( new Map<string,BenElement>(kvs) )
    let rec parseKv acc = function
        | BenString(String(k), BenEl(v,tail)) -> parseKv ((k,v)::acc) tail
        | End(tail) -> Some(buildMap acc, tail)
        | _ -> None
    chars
        |> prefixed 'd'
        |> Option.bind (parseKv [])

and (|BenEl|_|) = function
    | BenInt(x,tail) 
    | BenList(x,tail) 
    | BenDic(x, tail)
    | BenString(x,tail) -> Some(x,tail)
    | _ -> None


