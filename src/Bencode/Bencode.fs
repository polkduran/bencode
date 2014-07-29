namespace Bencode
open BencodeParser

module BenDecode =

    /// Parses a list of chars as a Bencode element
    let parseChars chars = 
        match chars with
        | BenEl(el, []) -> el
        | BenEl(el, tail) -> sprintf "Bad format at the ending of the bencode string %A" tail |> failwith
        | _ -> failwith "Cannot parse the bencode string"

    /// Parses a string as a Bencode element
    let parseString (bencodeString:string) = 
            bencodeString |> List.ofSeq |> parseChars
    
