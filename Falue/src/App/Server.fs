module Server

open System.Net
open System.Net.Sockets
open System.IO
open System
open System.Text
open System.Collections.Generic

let closeSocket (c: TcpClient) (sr: StreamReader) (sw: StreamWriter) =
    sr.Close()
    sw.Close()
    c.Close()

let rec serve (c: TcpClient) (sr: StreamReader) (sw: StreamWriter) (server: Storage.server) =
    async {
        match sr.ReadLine() with
        | null -> closeSocket c sr sw
        | line ->
            match line with
            | "quit" -> closeSocket c sr sw
            | _ ->
                let msg =
                    match Parser.parse line with
                    | Ok query -> query.ToString()
                    | Error msg -> msg

                let a = Parser.KeyString "key"
                server.Set((a, Parser.StringV "value"))
                let res = server.Fetch(a)
                printfn "%A" res

                sw.WriteLine(msg)
                return! serve c sr sw server
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


// let addr = IPAddress.Parse("127.0.0.1")
// let listener = new TcpListener(addr, 2000)
// listener.Start()

// // listener.Start()
// // printfn "%i is the port" port

// // while true do
// //     Console.WriteLine "Waiting for connection..."
// // listener.Server.Accept() |> writeToSocket
