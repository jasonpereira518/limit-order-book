(** Randomized market-event generators and invariant checks for Stage 4.

    Events are *abstract* (no concrete order ids). The [replay] function assigns
    monotonic ids/timestamps so generators stay shrink-friendly and do not have
    to invent valid references into a live book. Cancel/modify pick a resting
    order by [slot % length], which always names something real when the book
    is non-empty (no-op when empty). *)

open Core
open Orderbook

module Action = struct
  type t =
    | Submit of
        { side : Side.t
        ; order_type : Order_type.t
        ; price : int
        ; qty : int
        }
    | Cancel of { slot : int }
    | Modify_down of
        { slot : int
        ; new_qty : int
        }
  [@@deriving sexp]
end

module Counters = struct
  type t =
    { submitted : int
    ; (** Sum of [Fill.quantity] — traded volume, counted once per trade. *)
      traded : int
    ; canceled : int
    }
  [@@deriving sexp, compare]

  let zero = { submitted = 0; traded = 0; canceled = 0 }
end

module State = struct
  type t =
    { book : Book.t
    ; next_id : int
    ; next_ts : int
    ; counters : Counters.t
    ; (** order id → arrival timestamp, for price-time checks on fills *)
      arrival : int Map.M(Int).t
    }

  let empty =
    { book = Book.empty
    ; next_id = 1
    ; next_ts = 1
    ; counters = Counters.zero
    ; arrival = Map.empty (module Int)
    }
  ;;
end

let gen_side : Side.t Quickcheck.Generator.t =
  Quickcheck.Generator.of_list [ Side.Buy; Side.Sell ]
;;

let gen_order_type : Order_type.t Quickcheck.Generator.t =
  Quickcheck.Generator.weighted_union
    [ 5.0, Quickcheck.Generator.return Order_type.Limit
    ; 1.0, Quickcheck.Generator.return Order_type.Market
    ; 1.0, Quickcheck.Generator.return Order_type.Ioc
    ; 1.0, Quickcheck.Generator.return Order_type.Fok
    ; 1.0, Quickcheck.Generator.return Order_type.Post_only
    ]
;;

let gen_action : Action.t Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let gen_submit =
    let%map side = gen_side
    and order_type = gen_order_type
    and price = Int.gen_incl 95 105
    and qty = Int.gen_incl 1 8 in
    Action.Submit { side; order_type; price; qty }
  in
  let gen_cancel =
    let%map slot = Int.gen_incl 0 32 in
    Action.Cancel { slot }
  in
  let gen_modify =
    let%map slot = Int.gen_incl 0 32
    and new_qty = Int.gen_incl 1 8 in
    Action.Modify_down { slot; new_qty }
  in
  Quickcheck.Generator.weighted_union
    [ 6.0, gen_submit; 2.0, gen_cancel; 1.0, gen_modify ]
;;

let gen_actions : Action.t list Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let%bind length = Int.gen_incl 1 30 in
  Quickcheck.Generator.list_with_length length gen_action
;;

let actions_shrinker : Action.t list Quickcheck.Shrinker.t =
  (* Drop one event at a time — the classic list shrinker. Atomic on each
     [Action.t] so shrinking focuses on shortening the sequence. *)
  Quickcheck.Shrinker.create (fun actions ->
    let n = List.length actions in
    Sequence.init n ~f:(fun i -> List.filteri actions ~f:(fun j _ -> j <> i)))
;;

let fills_respect_price_time
    ~(aggressor_side : Side.t)
    ~(fills : Fill.t list)
    ~(arrival : int Map.M(Int).t)
  =
  let prices = List.map fills ~f:(fun (f : Fill.t) -> f.price) in
  let price_ok =
    match aggressor_side with
    | Side.Buy -> List.is_sorted prices ~compare:Price.compare
    | Side.Sell ->
      List.is_sorted prices ~compare:(fun a b -> Price.compare b a)
  in
  let time_ok =
    fills
    |> List.group ~break:(fun (a : Fill.t) (b : Fill.t) ->
      not (Price.equal a.price b.price))
    |> List.for_all ~f:(fun group ->
      let tss =
        List.map group ~f:(fun (f : Fill.t) ->
          Map.find_exn arrival (Order_id.to_int f.maker_id))
      in
      List.is_sorted tss ~compare:Int.compare)
  in
  price_ok && time_ok
;;

let pick_resting book slot =
  let resting = Book.resting_orders book in
  match resting with
  | [] -> None
  | _ ->
    let i = Int.rem slot (List.length resting) in
    (* Int.rem can be negative in OCaml if slot is negative; our gen is >= 0. *)
    List.nth resting i
;;

let apply_action (state : State.t) (action : Action.t) : State.t =
  match action with
  | Cancel { slot } ->
    (match pick_resting state.book slot with
     | None -> state
     | Some (order : Order.t) ->
       let book = Book.cancel state.book order.id |> Or_error.ok_exn in
       let counters =
         { state.counters with
           canceled = state.counters.canceled + Quantity.to_int order.quantity
         }
       in
       { state with book; counters })
  | Modify_down { slot; new_qty } ->
    (match pick_resting state.book slot with
     | None -> state
     | Some (order : Order.t) ->
       let old = Quantity.to_int order.quantity in
       let new_qty = Int.min new_qty (old - 1) in
       if new_qty <= 0
       then state
       else (
         let book =
           Book.modify_in_place
             state.book
             order.id
             ~new_qty:(Quantity.of_int_exn new_qty)
           |> Or_error.ok_exn
         in
         let counters =
           { state.counters with canceled = state.counters.canceled + (old - new_qty) }
         in
         { state with book; counters }))
  | Submit { side; order_type; price; qty } ->
    let id = state.next_id in
    let ts = state.next_ts in
    let order
        : Order.t =
      { id = Order_id.of_int id
      ; side
      ; price = Price.of_int_exn price
      ; quantity = Quantity.of_int_exn qty
      ; timestamp = Timestamp.of_int ts
      ; order_type
      }
    in
    let result = Engine.submit state.book order in
    let state =
      { state with
        next_id = id + 1
      ; next_ts = ts + 1
      ; arrival = Map.set state.arrival ~key:id ~data:ts
      }
    in
    (match result.status with
     | Rejected _ ->
       (* Rejected orders never enter the conservation ledger. *)
       { state with book = result.book }
     | Accepted ->
       let traded_delta =
         List.fold result.fills ~init:0 ~f:(fun acc (f : Fill.t) ->
           acc + Quantity.to_int f.quantity)
       in
       let canceled_delta =
         match order_type with
         | Order_type.Market | Ioc -> Quantity.to_int result.remaining_qty
         | Limit | Fok | Post_only -> 0
       in
       if not
            (fills_respect_price_time
               ~aggressor_side:side
               ~fills:result.fills
               ~arrival:state.arrival)
       then
         failwith
           (Sexp.to_string_hum
              [%message
                "price-time priority violated"
                  (action : Action.t)
                  (result.fills : Fill.t list)])
       else (
         let counters =
           { Counters.submitted = state.counters.submitted + qty
           ; traded = state.counters.traded + traded_delta
           ; canceled = state.counters.canceled + canceled_delta
           }
         in
         { state with book = result.book; counters }))
;;

let replay (actions : Action.t list) : State.t =
  List.fold actions ~init:State.empty ~f:apply_action
;;

let assert_invariants (state : State.t) =
  if Book.is_crossed state.book
  then failwith "book is crossed (best bid >= best ask)";
  let resting = Quantity.to_int (Book.total_resting_qty state.book) in
  let { Counters.submitted; traded; canceled } = state.counters in
  (* Each trade removes [qty] from the aggressor *and* [qty] from a resting
     order — both units were counted in [submitted]. So traded volume appears
     twice on the right-hand side:
     [submitted = resting + canceled + 2 * traded]. *)
  if submitted <> resting + canceled + (2 * traded)
  then
    failwith
      (Sexp.to_string_hum
         [%message
           "quantity not conserved"
             (submitted : int)
             (resting : int)
             (traded : int)
             (canceled : int)
             ~expected_rhs:(resting + canceled + (2 * traded) : int)])
;;

let check_actions actions =
  let state = replay actions in
  assert_invariants state
;;
