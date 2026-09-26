(** CLI: generate synthetic flow and benchmark the matching engine. *)

open Core
open Orderbook

let run_bench ~events ~seed ~fair =
  printf "Generating %d events (seed=%d, fair=%d)…\n%!" events seed fair;
  let gen_start = Time_ns.now () in
  let config =
    { Simulator.default_config with num_events = events; seed; initial_fair = fair }
  in
  let flow = Simulator.generate config in
  let gen_span = Time_ns.diff (Time_ns.now ()) gen_start in
  printf "Generated in %s\n%!" (Time_ns.Span.to_string_hum gen_span);
  printf "Replaying through matching engine…\n%!";
  let stats = Bench.run flow in
  printf "\n=== Benchmark results ===\n";
  Bench.print_stats stats;
  printf
    "\nNotes:\n\
     - Book is Core.Map price levels + Fqueue FIFO; expect O(log P) level ops.\n\
     - Cancel-in-level still scans the Fqueue (O(L)); hot path for deep levels.\n\
     - p50 often reads 0 ns: Time_ns quantizes very short intervals on this host.\n\
       Prefer mean / p95 / p99 and throughput for comparisons.\n\
     - Latency max spikes are typically GC; this is not a low-GC trading binary.\n"
;;

let command =
  Command.basic
    ~summary:"Limit order book simulator + matching-engine benchmark"
    (let%map_open.Command events =
       flag
         "-events"
         (optional_with_default 1_000_000 int)
         ~doc:"INT number of synthetic events to generate/replay (default 1000000)"
     and seed =
       flag
         "-seed"
         (optional_with_default 42 int)
         ~doc:"INT PRNG seed (default 42)"
     and fair =
       flag
         "-fair"
         (optional_with_default 10_000 int)
         ~doc:"INT initial fair-value tick (default 10000)"
     in
     fun () -> run_bench ~events ~seed ~fair)
;;

let () = Command_unix.run command
