(** Benchmark harness: replay synthetic flow through the matching engine.

    Timing uses [Time_ns] (nanosecond clock). Per-event samples feed
    p50/p95/p99; throughput comes from total wall time.

    The harness keeps an incremental index of resting order ids so cancel/modify
    do not call [Book.resting_orders] (full-book scan) on every event. *)

open Core
open Types

type stats =
  { num_events : int
  ; wall : Time_ns.Span.t
  ; throughput : float
  ; mean : Time_ns.Span.t
  ; p50 : Time_ns.Span.t
  ; p95 : Time_ns.Span.t
  ; p99 : Time_ns.Span.t
  ; p_max : Time_ns.Span.t
  ; accepted : int
  ; rejected : int
  ; trades : int
  ; resting_orders : int
  ; best_bid : int option
  ; best_ask : int option
  }

let to_orderbook_side = function
  | Simulator.Buy -> Side.Buy
  | Simulator.Sell -> Side.Sell
;;

let to_orderbook_type = function
  | Simulator.Limit -> Order_type.Limit
  | Simulator.Market -> Order_type.Market
  | Simulator.Ioc -> Order_type.Ioc
  | Simulator.Fok -> Order_type.Fok
  | Simulator.Post_only -> Order_type.Post_only
;;

(** Resting-id index: dynarray for O(1) slot pick + hashtable for O(1) remove. *)
type resting_index =
  { ids : int Dynarray.t
  ; pos : int Hashtbl.M(Int).t
  }

let resting_create () : resting_index =
  { ids = Dynarray.create (); pos = Hashtbl.create (module Int) }
;;

let resting_add (t : resting_index) (id : Order_id.t) =
  let id = Order_id.to_int id in
  if not (Hashtbl.mem t.pos id)
  then (
    Hashtbl.set t.pos ~key:id ~data:(Dynarray.length t.ids);
    Dynarray.add_last t.ids id)
;;

let resting_remove (t : resting_index) (id : Order_id.t) =
  let id = Order_id.to_int id in
  match Hashtbl.find_and_remove t.pos id with
  | None -> ()
  | Some i ->
    let last_i = Dynarray.length t.ids - 1 in
    if i < last_i
    then (
      let moved = Dynarray.get t.ids last_i in
      Dynarray.set t.ids i moved;
      Hashtbl.set t.pos ~key:moved ~data:i);
    Dynarray.truncate t.ids last_i
;;

let resting_pick (t : resting_index) slot =
  let n = Dynarray.length t.ids in
  if n = 0 then None else Some (Order_id.of_int (Dynarray.get t.ids (slot % n)))
;;

let resting_length (t : resting_index) = Dynarray.length t.ids

type replay_state =
  { book : Book.t
  ; next_id : int
  ; next_ts : int
  ; accepted : int
  ; rejected : int
  ; trades : int
  ; resting : resting_index
  }

let empty_state =
  { book = Book.empty
  ; next_id = 1
  ; next_ts = 1
  ; accepted = 0
  ; rejected = 0
  ; trades = 0
  ; resting = resting_create ()
  }
;;

let apply_event (state : replay_state) (event : Simulator.event) : replay_state =
  match event with
  | Cancel { slot } ->
    (match resting_pick state.resting slot with
     | None -> state
     | Some id ->
       (match Book.cancel state.book id with
        | Error _ -> state
        | Ok book ->
          resting_remove state.resting id;
          { state with book }))
  | Modify_down { slot; new_qty } ->
    (match resting_pick state.resting slot with
     | None -> state
     | Some id ->
       (match Book.find state.book id with
        | None -> state
        | Some (order : Order.t) ->
          let old = Quantity.to_int order.quantity in
          let new_qty = Int.min new_qty (old - 1) in
          if new_qty <= 0
          then state
          else (
            match
              Book.modify_in_place state.book id ~new_qty:(Quantity.of_int_exn new_qty)
            with
            | Error _ -> state
            | Ok book -> { state with book })))
  | Submit { side; order_type; price; qty } ->
    let order
        : Order.t =
      { id = Order_id.of_int state.next_id
      ; side = to_orderbook_side side
      ; price = Price.of_int_exn price
      ; quantity = Quantity.of_int_exn qty
      ; timestamp = Timestamp.of_int state.next_ts
      ; order_type = to_orderbook_type order_type
      }
    in
    let result = Engine.submit state.book order in
    List.iter result.fills ~f:(fun (f : Fill.t) ->
      resting_remove state.resting f.maker_id);
    if Book.mem result.book order.id then resting_add state.resting order.id;
    let accepted, rejected =
      match result.status with
      | Accepted -> state.accepted + 1, state.rejected
      | Rejected _ -> state.accepted, state.rejected + 1
    in
    { book = result.book
    ; next_id = state.next_id + 1
    ; next_ts = state.next_ts + 1
    ; accepted
    ; rejected
    ; trades = state.trades + List.length result.trades
    ; resting = state.resting
    }
;;

let percentile_ns (sorted : int array) ~(pct : int) =
  let n = Array.length sorted in
  if n = 0
  then Time_ns.Span.zero
  else (
    let idx = Int.min (n - 1) (Int.max 0 (((pct * (n - 1)) + 99) / 100)) in
    Time_ns.Span.of_int_ns sorted.(idx))
;;

let run ?(compact_gc = true) (events : Simulator.event array) : stats =
  if compact_gc then Gc.compact ();
  let n = Array.length events in
  let latencies = Array.create ~len:n 0 in
  let state = ref empty_state in
  let wall_start = Time_ns.now () in
  for i = 0 to n - 1 do
    let t0 = Time_ns.now () in
    state := apply_event !state events.(i);
    let t1 = Time_ns.now () in
    latencies.(i) <- Time_ns.Span.to_int_ns (Time_ns.diff t1 t0)
  done;
  let wall = Time_ns.diff (Time_ns.now ()) wall_start in
  let sum = Array.fold latencies ~init:0 ~f:(fun acc x -> acc + x) in
  Array.sort latencies ~compare:Int.compare;
  let wall_s = Time_ns.Span.to_sec wall in
  let throughput = if Float.(wall_s <= 0.) then 0. else Float.of_int n /. wall_s in
  let final = !state in
  { num_events = n
  ; wall
  ; throughput
  ; mean = Time_ns.Span.of_int_ns (sum / Int.max 1 n)
  ; p50 = percentile_ns latencies ~pct:50
  ; p95 = percentile_ns latencies ~pct:95
  ; p99 = percentile_ns latencies ~pct:99
  ; p_max = percentile_ns latencies ~pct:100
  ; accepted = final.accepted
  ; rejected = final.rejected
  ; trades = final.trades
  ; resting_orders = resting_length final.resting
  ; best_bid = Book.best_bid final.book |> Option.map ~f:Price.to_int
  ; best_ask = Book.best_ask final.book |> Option.map ~f:Price.to_int
  }
;;

let print_stats (s : stats) =
  let ns span = Time_ns.Span.to_int_ns span in
  printf "events:          %d\n" s.num_events;
  printf "wall:            %s\n" (Time_ns.Span.to_string_hum s.wall);
  printf "throughput:      %.0f events/sec\n" s.throughput;
  printf "latency mean:    %d ns\n" (ns s.mean);
  printf "latency p50:     %d ns\n" (ns s.p50);
  printf "latency p95:     %d ns\n" (ns s.p95);
  printf "latency p99:     %d ns\n" (ns s.p99);
  printf "latency max:     %d ns\n" (ns s.p_max);
  printf "accepted:        %d\n" s.accepted;
  printf "rejected:        %d\n" s.rejected;
  printf "trades:          %d\n" s.trades;
  printf "resting orders:  %d\n" s.resting_orders;
  printf
    "best bid/ask:    %s / %s\n"
    (Option.value_map s.best_bid ~default:"-" ~f:Int.to_string)
    (Option.value_map s.best_ask ~default:"-" ~f:Int.to_string);
;;
