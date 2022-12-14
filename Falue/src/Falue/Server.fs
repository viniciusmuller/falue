module Server

open System.Net
open System.Net.Sockets
open System.IO
open System
open System.Text
open System.Collections.Generic

open Falue.Api

let closeSocket (c: TcpClient) (sr: StreamReader) (sw: StreamWriter) =
    sr.Close()
    sw.Close()
    c.Close()

let extractValue =
    function
    | StringV a -> a
    | IntV a -> string a
    | FloatV a -> string a

let extractKey =
    function
    | KeyString a -> a
    | KeyInt a -> string a

let processLine line (server: Storage.server) =
    match Parser.parse line with
    | Ok command ->
        match command with
        | Set kv ->
            server.Set(kv)
            "()"
        | Get key ->
            server.Fetch(key)
            |> Option.map extractValue
            |> Option.defaultValue ":notfound"
        | Remove k ->
            server.Remove(k)
            "()"
        | ListKeys ->
            server.ListKeys()
            |> List.map extractKey
            |> String.concat "\n"
    | Error msg -> msg

type Option<'T> =
    private
    | Option of 'T
    static member ofNull =
        function
        | null -> None
        | x -> Some x

let rec serve (c: TcpClient) (sr: StreamReader) (sw: StreamWriter) (server: Storage.server) =
    async {
        let parseResult = Option.ofNull (sr.ReadLine())

        match parseResult with
        | Some line ->
            match line with
            | "quit" -> closeSocket c sr sw
            | _ ->
                let msg = processLine line server
                sw.WriteLine(msg)
                return! serve c sr sw server
        | None -> closeSocket c sr sw
    }


let loop (listener: TcpListener) =
    async {
        let server = Storage.server ()

        while true do
            let c = listener.AcceptTcpClient()
            let d = c.GetStream()
            let sr = new StreamReader(d)
            let sw = new StreamWriter(d)
            sw.AutoFlush <- true
            Async.Start(serve c sr sw server)
    }

let startListening port =
    let local = IPAddress.Parse "127.0.0.1"
    let listener = new TcpListener(localaddr = local, port = port)
    listener.Start()
    Async.RunSynchronously(loop listener)
