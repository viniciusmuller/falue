# Falue

Falue is an in-progress in-memory key-value store application.

It offers a TCP interface and guarantees strong data consistency by using the
actor model to query reads and writes.

# Usage
```shell
[nix-shell:~/projects/falue/Falue/src/App]$ telnet localhost 8080
set a = 1
:ok
set 2342 = 23432
:ok
set b = "strings"
:ok
listkeys
2342
a
b
get b
strings
```

# Contributing
You can run the tests with `dotnet test` inside the `Falue.Tests` project

# TODO
- Implement TTL
- Implement client library
- Add `select` command
