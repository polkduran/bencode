namespace Bencode

/// Bencode Elements
type BenElement =
    | Int of int64
    | String of string
    | List of BenElement list
    | Dict of Map<string,BenElement>   

module private BencodeParser =
    open System

    /// Reverses a list of chars and convert it in to a String
    let toStringRev chars = System.String(chars |> List.rev |> Array.ofSeq)

    /// Reverses a list of chars and convert it in to a Int64
    let toInt = toStringRev >> int64

    /// Returns the tail of a list if it begins by a given prefix
    let prefixed prefix = function 
                    | head::tail when head = prefix -> Some(tail)
                    | _ -> None

    /// Numeric representation of a char
    let (|Num|_|) (c:char) = if c >= '0' && c <= '9' then Some(c) else None

    /// Ends with 'e' (bencode ending char)
    let (|End|_|) = function 'e'::tail -> Some(tail) |_ -> None

    /// Returns a Int64 option consuming all the elements of a char list until a given suffix
    let tryGetNums suffix chars =
        let rec parseInternal acc = function
                |Num(c)::s::tail when s = suffix -> Some(toInt (c::acc), tail)
                |Num(c)::tail -> parseInternal (c::acc) tail
                |_ -> None
        match chars with
            |'-'::'0'::tail -> None // cannot begin by -0
            |'-'::tail -> parseInternal ['-'] tail // negative number
            |_ as tail -> parseInternal [] tail

    (*
        BenElements active patterns:
        match a list of chars against a BenElement and the rest of the char list            
    *)

    /// Bencode Int match (i{Int64}e), number can be negative, positive or zero
    let (|BenInt|_|) chars =
        chars 
            |> prefixed 'i'
            |> Option.bind (tryGetNums 'e')
            |> Option.map (fun (i,tail) -> Int i,tail)
        
    /// Bencode String match (n:{str}) where n is the length of the string {str}
    let (|BenString|_|) chars =
        /// Parses a string according to its length
        let rec parseString acc = function
            | n,s::tail when n > 0L -> parseString  (s::acc) (n-1L, tail)
            | n,_ when n > 0L -> None
            | _,rest -> Some(acc |> toStringRev |> BenElement.String, rest)
        chars 
            |> tryGetNums ':' // length of the string
            |> Option.bind (parseString [])
 
    /// Bencode list (l{content}e), where {content} can be any other BenElement
    let rec (|BenList|_|) chars =
        let rec parseList acc = function
            |BenEl(x,tail) -> parseList (x::acc) tail
            |End(tail) -> Some( List(acc |> List.rev), tail)
            |_ -> None
        chars
            |> prefixed 'l'
            |> Option.bind (parseList [])

    /// Bencode dictionary (d{content}e) where content is a sequence of key value pairs (bencode string, bencode element)
    and (|BenDic|_|) chars =
        let buildMap kvs = Dict( new Map<string,BenElement>(kvs) )
        let rec parseKv acc = function
            | BenString(String(k), BenEl(v,tail)) -> parseKv ((k,v)::acc) tail
            | End(tail) -> Some(buildMap acc, tail)
            | _ -> None
        chars
            |> prefixed 'd'
            |> Option.bind (parseKv [])

    /// Any Bencode element
    and (|BenEl|_|) = function
        | BenInt(x,tail) 
        | BenList(x,tail) 
        | BenDic(x, tail)
        | BenString(x,tail) -> Some(x,tail)
        | _ -> None

