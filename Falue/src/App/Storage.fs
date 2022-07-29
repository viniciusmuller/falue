module Storage

type StorageStructure = Map<Parser.Key, Parser.Value>

type Message =
    | Stop
    | Set of (Parser.Key * Parser.Value)
    | Fetch of (AsyncReplyChannel<Parser.Value option> * Parser.Key)

let add = Map.add
let get = Map.tryFind
let remove = Map.remove

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
                }

            loop Map.empty)

    member this.Set(x) = innerServer.Post(Set x)

    member this.Fetch(key) =
        innerServer.PostAndReply((fun reply -> Fetch(reply, key)), timeout = 2000)

    member this.Stop() = innerServer.Post(Stop)
