open FParsec

type Value =
    | IntV of int
    | StringV of string
    | FloatV of float

// type Query =
//     { key: Key option
//       ttl: int
//       value: Value option }

// static member Default = { key = None; value = None; ttl = 300 }

type Parser<'t> = Parser<'t, unit>

// ---------- helpers
let ws = spaces
let str = pstring
let strWs s = pstring s .>> ws

// ---------- symbols
let equals = strWs "="
let separator = strWs ";"

// ---------- data
let digit = isAnyOf [ '0' .. '9' ]
let intP = many1Satisfy digit |>> (int >> IntV)

let word =
    asciiLetter .>>. (many asciiLetter)
    |>> fun (i, j) -> StringV(string (i :: j))

// TODO: Figure out how to get the data out
// >>. updateUserState (fun (x) -> Reply(x))

let quote = pchar '"' <|> pchar '\''
let strP = between quote quote word

let floatP = pfloat |>> FloatV

let keyP = ws >>. (strP <|> intP) .>> ws

let ttl = strWs "ttl" .>> equals .>> puint32

let options = choice [ ttl ]

let valueP = ws >>. (strP <|> intP <|> floatP) .>> ws

let grammar =
    strWs "set" >>. keyP .>> equals >>. valueP
    .>> strWs "with"
    .>> (sepBy options separator)

let parser: Parser<_> = grammar

let result = run parser @"set 'rodrigo' = 3423 with ttl = 1010"

match result with
| Success (result, s, pos) -> printfn "%A %A %A" result s pos
| Failure (msg, error, _) -> printfn "%s %A" msg error
