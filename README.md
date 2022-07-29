# Falue

Falue is an in-progress in-memory key-value store application.

## Usage
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

# TODO
- Implement TTL
- Implement client library
- Add `select` command
