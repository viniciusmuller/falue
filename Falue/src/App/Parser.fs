module Parser

open FParsec
open System

type Value =
    | IntV of int
    | StringV of string
    | FloatV of float

type Option =
    | TTL of uint32
    | Host of string

type Command =
    | Set of (Value * Value)
    | Get of Value
    | ListKeys

type private Parser<'t> = Parser<'t, unit>

let private charListToString = Array.ofList >> String

// ---------- helpers
let private ws = spaces
let private str = pstring
let private strWs s = pstring s .>> ws

// ---------- symbols
let private equals = ws >>. strWs "="
let private separator = strWs ";"

// ---------- data
let private intP = many1 digit |>> (charListToString >> int >> IntV)

let private identifier =
    asciiLetter
    .>>. (many (letter <|> digit <|> anyOf ".@"))
    |>> fun (i, j) -> charListToString (i :: j)

let private identifierP = identifier |>> StringV

let private digitOrLetterString =
    many (asciiLetter <|> letter)
    |>> (charListToString >> StringV)

let private quote = choice [ pchar '"'; pchar '\'' ]
let private strP = between quote quote digitOrLetterString
let private floatP = pfloat |>> FloatV

let private ttl = strWs "ttl" >>. equals >>. puint32 |>> TTL

let private hostName =
    strWs "hostname" >>. equals >>. identifier
    |>> Host

let private options = choice [ ttl; hostName ]

let private keyP = ws >>. choice [ strP; identifierP; intP ]
let private valueP = ws >>. choice [ strP; intP; floatP ]

// TODO: Use the key type again
let private parseSet =
    tuple2 (strWs "set" >>. keyP .>> equals) valueP
    |>> Set

let private parseGet = strWs "get" >>. keyP |>> Get
let private parseList = strWs "listkeys" |>> (fun _ -> ListKeys)

// TODO: Figure out how to make this optional but raise error if it's present
// and it fails to parse
let private commandOptions = opt (ws >>. strWs "with" >>. sepBy options separator)

let private grammar = parseSet <|> parseGet <|> parseList

let parse input =
    let parser: Parser<_> = grammar

    match run parser input with
    | Success (query, _, _) -> FSharp.Core.Ok query
    | Failure (msg, error, _) -> FSharp.Core.Error msg
