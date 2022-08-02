module Falue.Tests.Parser

open NUnit.Framework

open Falue.Api

let mkExpected (t: 'a) : Result<'a, string> = Ok t

[<SetUp>]
let Setup () = ()

[<Test>]
let ``can parse set statements with identifier as key`` () =
    let query = "set a = 1"
    let expected = mkExpected (Set(KeyString "a", IntV 1))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse set statements with strings as key`` () =
    let query = "set \"string\" = 1"
    let expected = mkExpected (Set(KeyString "string", IntV 1))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse listkeys statement`` () =
    let query = "listkeys"
    let expected = mkExpected ListKeys
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse get statement with identifier as key`` () =
    let query = "get identifier"
    let expected = mkExpected (Get(KeyString "identifier"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse insert get with strings as key`` () =
    let query = "get \"string\""
    let expected = mkExpected (Get(KeyString "string"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse remove with strings as key`` () =
    let query = "del \"string\""
    let expected = mkExpected (Remove(KeyString "string"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse remove with identifier as key`` () =
    let query = "del ident"
    let expected = mkExpected (Remove(KeyString "ident"))
    Assert.AreEqual(Parser.parse query, expected)
