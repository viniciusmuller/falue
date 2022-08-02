namespace Falue.Api

type Key =
    | KeyString of string
    | KeyInt of int

type Value =
    | IntV of int
    | StringV of string
    | FloatV of float

type Option =
    | TTL of uint32
    | Host of string

type Command =
    | Set of (Key * Value)
    | Get of Key
    | Remove of Key
    | ListKeys

