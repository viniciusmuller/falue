module Falue.Client

open System
open System.Net.Sockets
open System.Net.Sockets
open System.IO

open Falue.Api

type Message =
    | Stop
    | Add of AsyncReplyChannel<unit> * (Key * Value)
    | Remove of AsyncReplyChannel<unit> * Key
    | Fetch of AsyncReplyChannel<Value option> * Key
    | ListKeys of AsyncReplyChannel<Key list>

type ClientState =
    { credentials: string * int // host, port
      socket: TcpClient }

let private keyToStr key =
    match key with
    | KeyInt i -> string i
    | KeyString s -> s

let private valueToStr value =
    match value with
    | IntV i -> string i
    | StringV s -> s
    | FloatV s -> string s


// TODO: Should read read server response and return Result<(), string>
let private add key value (writer: StreamWriter) =
    let key = keyToStr key
    let value = valueToStr value
    // TODO: possible security issue related to formatting queries,
    // maybe drop identifier support and only allow strings?
    let query = $"set {key} = {value}"
    writer.WriteLine(query)

let private get key (writer: StreamWriter) (reader: StreamWriter) =
    let key = keyToStr key
    let query = $"set {key}"
    let data = System.Text.Encoding.ASCII.GetBytes(query)
    data

let private remove = Map.remove
let private lsKeys map = Map.toList map |> List.map fst

type client() =
    let innerServer =
        MailboxProcessor.Start (fun inbox ->
            let rec loop (writer, reader) =
                async {
                    let! msg = inbox.Receive()
                    return! loop state

                    match msg with
                    | Stop -> return ()
                    | Fetch (reply, key) ->
                        reply.Reply(get key writer reader)
                        return! loop state
                    | ListKeys (reply) ->
                        reply.Reply(lsKeys state.socket)
                        return! loop storage
                    | Remove (reply, key) ->
                        let newState = remove key state.socket
                        reply.Reply(())
                        return! loop state
                    | Add (reply, (key, value)) ->
                        let newState = add key value state.socket
                        reply.Reply(())
                        return! loop state
                }

            loop Map.empty)

    member this.Set(x) =
        innerServer.PostAndReply((fun reply -> Add(reply, x)), timeout = 2000)

    member this.Remove(x) =
        innerServer.PostAndReply((fun reply -> Remove(reply, x)), timeout = 2000)

    member this.Fetch(key) =
        innerServer.PostAndReply((fun reply -> Fetch(reply, key)), timeout = 2000)

    member this.ListKeys() =
        innerServer.PostAndReply((fun reply -> ListKeys(reply)), timeout = 2000)

    member this.Stop() = innerServer.Post(Stop)
