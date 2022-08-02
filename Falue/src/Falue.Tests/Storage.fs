module Falue.Tests.Storage

open NUnit.Framework

open Falue.Api

[<SetUp>]
let Setup () = ()

let k = KeyString "foo"
let v = StringV "bar"

let remove k (server: Storage.server) = server.Remove(k)
let insert t (server: Storage.server) = server.Set(t)
let fetch k (server: Storage.server) = server.Fetch(k)

[<Test>]
let ``can insert data inside a server storage`` () =
    let server = Storage.server ()
    Assert.AreEqual((), insert (k, v) server)

[<Test>]
let ``can remove data from a server storage`` () =
    let server = Storage.server ()
    insert (k, v) server
    remove k server
    Assert.AreEqual(None, fetch k server)

[<Test>]
let ``can lookup data from a server storage`` () =
    let server = Storage.server ()
    insert (k, v) server
    let result = fetch k server
    Assert.AreEqual(Some v, result)
