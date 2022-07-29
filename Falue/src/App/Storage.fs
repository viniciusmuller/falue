module Storage

type StorageStructure = Map<Parser.Value, Parser.Value>

type Message =
    | Stop
    | Set of (Parser.Value * Parser.Value)
    | Fetch of (AsyncReplyChannel<Parser.Value option> * Parser.Value)
    | ListKeys of AsyncReplyChannel<Parser.Value list>

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
                    | Set (key, value) -> return! loop (add key value storage)
                    | Fetch (reply, key) ->
                        let value = get key storage
                        reply.Reply(value)
                        return! loop storage
                    | ListKeys (reply) ->
                        let keys = lsKeys storage
                        reply.Reply(keys)
                        return! loop storage
                }

            loop Map.empty)

    member this.Set(x) = innerServer.Post(Set x)

    member this.Fetch(key) =
        innerServer.PostAndReply((fun reply -> Fetch(reply, key)), timeout = 2000)

    member this.ListKeys() =
        innerServer.PostAndReply((fun reply -> ListKeys(reply)), timeout = 2000)

    member this.Stop() = innerServer.Post(Stop)
