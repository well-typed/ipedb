# Revision history for ipedb

## 0.2.0.1 -- 2026-07-16

- Fix issue where the `indexer` does not stop until the entire eventlog has been processed,
  rather than immediately after the relevant section.

## 0.2.0.0 -- 2026-07-02

This version is a complete reimplementation of the `ipedb` tool and library and adds the new `ccdb` tool.

- The database format is changed to `lsm-tree`, which substantially speeds up indexing.
  For instance, indexing the IPE entries for GHC goes down from 1-1/2 minutes to 7 seconds.
- The library exposes a more stable and database-agnostic API.
- The command-line interface for the `ipedb` tool is redesigned and includes listing all entries.

## 0.1.0.0 -- 2026-03-02

Experimental release of the `ipedb` tool and library, which build IPE databases from eventlog files using SQLite3.
