module Falue.Tests.Parser

open NUnit.Framework

let mkExpected (t: 'a) : Result<'a, string> = Ok t

[<SetUp>]
let Setup () = ()

[<Test>]
let ``can parse set statements with identifier as key`` () =
    let query = "set a = 1"
    let expected = mkExpected (Parser.Set(Parser.KeyString "a", Parser.IntV 1))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse set statements with strings as key`` () =
    let query = "set \"string\" = 1"
    let expected = mkExpected (Parser.Set(Parser.KeyString "string", Parser.IntV 1))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse listkeys statement`` () =
    let query = "listkeys"
    let expected = mkExpected Parser.ListKeys
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse get statement with identifier as key`` () =
    let query = "get identifier"
    let expected = mkExpected (Parser.Get(Parser.KeyString "identifier"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse insert get with strings as key`` () =
    let query = "get \"string\""
    let expected = mkExpected (Parser.Get(Parser.KeyString "string"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse remove with strings as key`` () =
    let query = "del \"string\""
    let expected = mkExpected (Parser.Remove(Parser.KeyString "string"))
    Assert.AreEqual(Parser.parse query, expected)

[<Test>]
let ``can parse remove with identifier as key`` () =
    let query = "del ident"
    let expected = mkExpected (Parser.Remove(Parser.KeyString "ident"))
    Assert.AreEqual(Parser.parse query, expected)
