(** Limit order book library.

    Stage 1: domain types at the library root (via [include Types]).
    Stage 2: resting [Book].
    Stage 3: [Engine] matching.
    Stage 4: property tests (see [test/]).
    Stage 5: human-readable [Render] for expect snapshots.
    Stage 6: [Simulator] + [Bench]. *)

open Core

include Types

module Book = Book
module Engine = Engine
module Render = Render
module Simulator = Simulator
module Bench = Bench

let name = "orderbook"
let version = "0.1.0"

let status () = sprintf "%s %s — complete" name version
