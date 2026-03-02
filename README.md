## ipedb

A tool and library for indexing GHC eventlog files and querying IPE (Info Table Provenance Entry) data.

### Overview

`ipedb` extracts and indexes IPE data from GHC eventlogs into a SQLite database, 
enabling efficient querying of heap profiling information. 
This is primarily useful for analyzing memory usage in Haskell programs compiled with `-finfo-table-map` and profiled with `-hi`.

### Usage

Build and run with a GHC eventlog file:

```
cabal run exe:ipedb -- <database.sqlite> index -f <program.eventlog>
cabal run exe:ipedb -- <database.sqlite> query -i 0x<hash>
```

### Background

IPE-based profiling allows mapping heap closures to source locations without requiring a profiled build. 
When compiled with `-finfo-table-map`, GHC embeds a mapping from info table pointers to source locations. 
The `-hi` RTS flag generates heap profiles broken down by info table address. 
`ipedb` indexes this data to enable efficient analysis of memory allocation patterns.

For more details on IPE profiling, see the [GHC User's Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/profiling.html) and the [Well-Typed blog post on Info Table Profiling](https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/).
