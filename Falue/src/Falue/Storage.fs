module Storage


open Falue.Api

type Message =
    | Stop
    | Add of AsyncReplyChannel<unit> * (Key * Value)
    | Remove of AsyncReplyChannel<unit> * Key
    | Fetch of AsyncReplyChannel<Value option> * Key
    | ListKeys of AsyncReplyChannel<Key list>

let add = Map.add
let get = Map.tryFind
let remove = Map.remove
let lsKeys map = Map.toList map |> List.map fst

type server() =
    let innerServer =
        MailboxProcessor.Start (fun inbox ->
            let rec loop storage =
                async {
                    let! msg = inbox.Receive()

                    match msg with
                    | Stop -> return ()
                    | Fetch (reply, key) ->
                        reply.Reply(get key storage)
                        return! loop storage
                    | ListKeys (reply) ->
                        reply.Reply(lsKeys storage)
                        return! loop storage
                    | Remove (reply, key) ->
                        let newState = remove key storage
                        reply.Reply(())
                        return! loop newState
                    | Add (reply, (key, value)) ->
                        let newState = add key value storage
                        reply.Reply(())
                        return! loop (newState)
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
