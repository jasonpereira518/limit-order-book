# Limit Order Book & Matching Engine (OCaml)

A price-time-priority (FIFO) limit order book and matching engine in OCaml, built with Jane Street’s `Core` / `ppx_jane` stack. It supports limit, market, IOC, FOK, and post-only orders plus cancel/modify, emits fills and a public trade tape, and is backed by property-based invariant tests, expect snapshots, a synthetic market-event simulator, and a latency/throughput benchmark harness.

## Architecture

```
                    ┌─────────────────────────────────────┐
                    │           bin/orderbook             │
                    │  simulator → bench → print stats    │
                    └─────────────────┬───────────────────┘
                                      │
          ┌───────────────────────────┼───────────────────────────┐
          ▼                           ▼                           ▼
   ┌─────────────┐            ┌──────────────┐            ┌─────────────┐
   │  Simulator  │            │    Engine    │            │    Bench    │
   │ random-walk │──events───▶│ match + rest │◀──replay───│ Time_ns     │
   │ fair value  │            │ order types  │            │ percentiles │
   └─────────────┘            └──────┬───────┘            └─────────────┘
                                     │
                                     ▼
                              ┌─────────────┐
                              │    Book     │
                              │ bids Map↓   │
                              │ asks Map↑   │
                              │ Fqueue FIFO │
                              │ by_id index │
                              └──────┬──────┘
                                     │
                                     ▼
                              ┌─────────────┐
                              │   Types     │
                              │ Side/Price/ │
                              │ Order/Fill/ │
                              │ Trade       │
                              └─────────────┘

   test/:  unit + expect snapshots + Base/Core Quickcheck invariants
```

**Matching rule:** walk the opposite side best→worse; within a price, FIFO. Trade price is the resting (maker) price. After an accepted match cycle the book must not cross (`best_bid < best_ask`, or a side empty).

## Build / test / benchmark

Requires OCaml ≥ 5.1 and an opam switch with the deps in `orderbook.opam`.

```bash
# one-time (if needed)
opam switch create orderbook 5.2.1   # or any >= 5.1
eval $(opam env)
opam install . --deps-only -y

# build
dune build

# all tests (unit + expect + Quickcheck + shrink demo)
dune runtest

# update expect-test goldens after intentional output changes
dune runtest --auto-promote

# benchmark (default: 1M synthetic events)
dune exec -- orderbook -events 1000000 -seed 42

# larger run
dune exec -- orderbook -events 2000000 -seed 42 -fair 10000
```

## Invariants tested (and why they matter)

Property tests replay randomized sequences of submit / cancel / modify and assert:

| Invariant | Check | Why it matters |
|---|---|---|
| **No crossed book** | `best_bid < best_ask` (or empty side) after every event | A crossed book means the matcher failed to take available liquidity — a hard correctness bug in any exchange-style engine. |
| **Quantity conservation** | `submitted = resting + canceled + 2 × traded` | Every share that entered must still be resting, cancelled, or traded. The `2×` factor exists because each trade removes qty from *both* the aggressor and a resting order (both previously counted in `submitted`). |
| **Price-time priority** | Fill prices walk monotonically for the aggressor; within a price, maker arrival times are non-decreasing | Ensures earlier (or better-priced) resting orders are not skipped — the economic fairness rule of a FIFO book. |

Expect tests additionally snapshot human-readable blotters (book + tape) for canonical scenarios (sweep two levels, FIFO at one price, FOK/post-only reject, IOC, etc.). A pedagogical shrink demo shows Quickcheck reducing a deliberate LIFO bug to a 3-event counterexample.

## Benchmark results

Measured on this project’s harness (`Time_ns` per-event samples; generation outside the timed loop). Host: Apple Silicon / macOS; seed `42`; simulator biased toward resting depth so the book is non-trivial (~7.6k resting orders at end of 1M).

| | 1M events | 2M events |
|---|---|---|
| **Throughput** | ~1.65M events/sec | ~1.61M events/sec |
| **Latency mean** | ~590 ns | ~600 ns |
| **p50** | 0 ns† | 0 ns† |
| **p95** | ~1 µs | ~1 µs |
| **p99** | ~2 µs | ~3 µs |
| **max** | ~0.25–0.5 ms (GC spikes) | ~0.5 ms |
| Resting at end | ~7.6k | ~16.5k |
| Trades | ~536k | ~1.07M |

†`Time_ns` often quantizes very short intervals to 0 on this host — prefer **mean / p95 / p99** and throughput.

**Honest interpretation:** solid for a clear `Core.Map` + `Fqueue` teaching implementation; not exchange-grade. Main costs to cite: `O(log P)` map ops per level, `O(L)` cancel-in-level via queue rebuild, and GC (visible in max latency). Contiguous level structures, pooling, and a low-allocation hot path would be the next performance steps (see stretch: OxCaml — only if pursued explicitly).

## Project layout

```
lib/types.ml       domain types (int ticks, never float prices)
lib/book.ml        resting book (Map levels + Fqueue + by_id)
lib/engine.ml      matching + order-type semantics
lib/render.ml      human blotter for expect tests
lib/simulator.ml   synthetic event flow
lib/bench.ml       replay + latency percentiles
bin/main.ml        CLI
test/              unit, expect, Quickcheck, shrink demo
```

## Resume bullet options

Pick one and edit to taste:

1. **Built a price-time-priority limit order book and matching engine in OCaml (Core/ppx_jane) supporting limit, market, IOC, FOK, and post-only orders; enforced no-cross and quantity-conservation invariants with Quickcheck over randomized event streams, and measured ~1.6M events/sec with ~2 µs p99 latency on a Map-based book.**

2. **Implemented FIFO matching with persistent `Map` price levels and property-based tests that shrink failures to minimal counterexamples; added expect-test blotter snapshots and a synthetic market simulator with `Time_ns` throughput/latency harness for reproducible performance reporting.**

3. **Designed an OCaml matching engine with explicit Fill vs public Trade models and dual modify semantics (in-place downsize vs cancel-reinsert); validated price-time priority under adversarial random order flow and documented Map/Fqueue bottlenecks against measured p95/p99.**
