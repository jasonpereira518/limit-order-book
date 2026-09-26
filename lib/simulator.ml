(** Synthetic market-event generator for benchmarks and load tests.

    Model (intentionally simple, “realistic-ish”):
    - A latent [fair] price follows a symmetric integer random walk.
    - Limit prices are fair + a small signed offset (liquidity around the touch).
    - Quantities are approximately log-normal (exp of a Gaussian), clipped to
      a tradable integer range.
    - Mix of submits (limit/market/ioc/fok/post-only), cancels, and downsizes.

    Events are abstract (no concrete ids); the bench/replay layer assigns
    monotonic ids and resolves cancel/modify slots against the live book. *)

open Core

type order_type =
  | Limit
  | Market
  | Ioc
  | Fok
  | Post_only
[@@deriving sexp]

type side =
  | Buy
  | Sell
[@@deriving sexp]

type event =
  | Submit of
      { side : side
      ; order_type : order_type
      ; price : int
      ; qty : int
      }
  | Cancel of { slot : int }
  | Modify_down of
      { slot : int
      ; new_qty : int
      }
[@@deriving sexp]

type config =
  { num_events : int
  ; seed : int
  ; initial_fair : int
  ; price_offset_max : int
  ; walk_step : int
  }

let default_config =
  { num_events = 1_000_000
  ; seed = 42
  ; initial_fair = 10_000
  ; price_offset_max = 8
  ; walk_step = 2
  }
;;

(** Approximate standard normal via average of 12 uniforms (CLT). *)
let std_normal (rng : Random.State.t) =
  let rec sum i acc =
    if i = 12
    then acc
    else sum (i + 1) (acc +. Random.State.float rng 1.)
  in
  sum 0 0. -. 6.
;;

let log_normal_qty rng ~mu ~sigma =
  let z = std_normal rng in
  let q = Float.exp (mu +. (sigma *. z)) |> Float.iround_nearest_exn in
  Int.clamp_exn q ~min:1 ~max:64
;;

let gen_side rng : side =
  if Random.State.bool rng then Buy else Sell
;;

let gen_event rng ~fair ~price_offset_max : event =
  match Random.State.int rng 20 with
  | 0 | 1 -> Cancel { slot = Random.State.int rng 64 }
  | 2 ->
    Modify_down
      { slot = Random.State.int rng 64; new_qty = 1 + Random.State.int rng 8 }
  | n ->
    let side = gen_side rng in
    (* Bias: most submits are non-crossing post-only / passive limits so the
       book accumulates depth. Occasional aggressive types create fills. *)
    let order_type =
      match n with
      | 3 -> Market
      | 4 -> Ioc
      | 5 -> Fok
      | 6 | 7 | 8 | 9 -> Post_only
      | _ -> Limit
    in
    let offset = Random.State.int rng (price_offset_max * 2 + 1) - price_offset_max in
    (* Passive limit/post-only away from fair so they rest more often. *)
    let price =
      match order_type with
      | Market -> 0
      | Post_only | Limit ->
        (match side with
         | Buy -> Int.max 1 (fair - 1 - Random.State.int rng price_offset_max)
         | Sell -> fair + 1 + Random.State.int rng price_offset_max)
      | Ioc | Fok -> Int.max 1 (fair + offset)
    in
    let qty = log_normal_qty rng ~mu:1.2 ~sigma:0.7 in
    Submit { side; order_type; price; qty }
;;

let step_fair rng fair ~walk_step =
  let delta = Random.State.int rng (walk_step * 2 + 1) - walk_step in
  Int.max 1 (fair + delta)
;;

let generate (config : config) : event array =
  let rng = Random.State.make [| config.seed |] in
  let fair = ref config.initial_fair in
  Array.init config.num_events ~f:(fun _ ->
    fair := step_fair rng !fair ~walk_step:config.walk_step;
    gen_event rng ~fair:!fair ~price_offset_max:config.price_offset_max)
;;
