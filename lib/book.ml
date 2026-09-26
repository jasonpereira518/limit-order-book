(** Resting limit order book: price levels + FIFO queues.

    Layout
    - Bids: [Map] keyed by price with a *descending* comparator, so the best
      (highest) bid is [Map.min_elt].
    - Asks: [Map] keyed by price ascending, so the best (lowest) ask is
      [Map.min_elt].
    - Each price level is a [Fqueue] of resting orders — enqueue at the back,
      dequeue from the front — which is exactly price-time (FIFO) priority.
    - [by_id] indexes every resting order's [(side, price)] so cancel/modify
      do not have to scan the whole book.

    This module only stores resting liquidity. Crossing / matching is Stage 3. *)

open Core
open Types

(** One price level: resting orders in arrival order (front = highest time priority). *)
type level = Order.t Fqueue.t [@@deriving sexp_of]

(** Bid keys sort high→low so [Map.min_elt] is the best bid. *)
module Bid_key = struct
  module T = struct
    type t = Price.t [@@deriving sexp]

    let compare a b = Price.compare b a
  end

  include T
  include Comparator.Make (T)
end

(** Ask keys sort low→high so [Map.min_elt] is the best ask. *)
module Ask_key = struct
  module T = struct
    type t = Price.t [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

module Id_key = struct
  module T = struct
    type t = Order_id.t [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { bids : level Map.M(Bid_key).t
  ; asks : level Map.M(Ask_key).t
  ; by_id : (Side.t * Price.t) Map.M(Id_key).t
  }
[@@deriving sexp_of]

let empty =
  { bids = Map.empty (module Bid_key)
  ; asks = Map.empty (module Ask_key)
  ; by_id = Map.empty (module Id_key)
  }
;;

let best_bid t = Map.min_elt t.bids |> Option.map ~f:fst
let best_ask t = Map.min_elt t.asks |> Option.map ~f:fst

(** Highest-priority resting order on [side] (best price, then FIFO front). *)
let best_order t side =
  match side with
  | Side.Buy ->
    (match Map.min_elt t.bids with
     | None -> None
     | Some (_, level) -> Fqueue.peek level)
  | Side.Sell ->
    (match Map.min_elt t.asks with
     | None -> None
     | Some (_, level) -> Fqueue.peek level)
;;

(** True when both sides exist and best bid >= best ask (an invariant violation
    after a completed match cycle). *)
let is_crossed t =
  match best_bid t, best_ask t with
  | Some bid, Some ask -> Price.compare bid ask >= 0
  | _ -> false
;;

let orders_at t side price =
  match side with
  | Side.Buy ->
    Map.find t.bids price |> Option.value_map ~default:[] ~f:Fqueue.to_list
  | Side.Sell ->
    Map.find t.asks price |> Option.value_map ~default:[] ~f:Fqueue.to_list
;;

let find t order_id =
  match Map.find t.by_id order_id with
  | None -> None
  | Some (side, price) ->
    orders_at t side price
    |> List.find ~f:(fun (o : Order.t) -> Order_id.equal o.id order_id)
;;

let mem t order_id = Map.mem t.by_id order_id

(** All resting orders (both sides). Order is unspecified across prices; within a
    price level, FIFO order is preserved. *)
let resting_orders t =
  let levels map = Map.data map |> List.concat_map ~f:Fqueue.to_list in
  levels t.bids @ levels t.asks
;;

let total_resting_qty t =
  resting_orders t
  |> List.fold ~init:Quantity.zero ~f:(fun acc (o : Order.t) -> acc + o.quantity)
;;

(** Price levels in matching order (best price first). Each level is FIFO. *)
let bid_levels t =
  Map.to_alist t.bids
  |> List.map ~f:(fun (price, level) -> price, Fqueue.to_list level)
;;

let ask_levels t =
  Map.to_alist t.asks
  |> List.map ~f:(fun (price, level) -> price, Fqueue.to_list level)
;;

let enqueue_at_level (level : level option) (order : Order.t) : level =
  let q = Option.value level ~default:Fqueue.empty in
  Fqueue.enqueue q order
;;

let remove_from_level (level : level) (order_id : Order_id.t) : level option =
  let remaining =
    Fqueue.to_list level
    |> List.filter ~f:(fun (o : Order.t) -> not (Order_id.equal o.id order_id))
    |> Fqueue.of_list
  in
  if Fqueue.is_empty remaining then None else Some remaining
;;

let update_in_level (level : level) (order_id : Order_id.t) ~(f : Order.t -> Order.t)
  : level Or_error.t
  =
  let list = Fqueue.to_list level in
  match List.find list ~f:(fun (o : Order.t) -> Order_id.equal o.id order_id) with
  | None ->
    Or_error.error_s
      [%message "order missing from price level" (order_id : Order_id.t)]
  | Some _ ->
    list
    |> List.map ~f:(fun (o : Order.t) ->
      if Order_id.equal o.id order_id then f o else o)
    |> Fqueue.of_list
    |> Ok
;;

let insert t (order : Order.t) =
  if order.quantity <= Quantity.zero
  then
    invalid_arg
      [%string
        "Book.insert: quantity must be positive, got %{Quantity.to_string order.quantity}"]
  else if Map.mem t.by_id order.id
  then
    invalid_arg
      [%string "Book.insert: duplicate order id %{Order_id.to_string order.id}"]
  else (
    let by_id = Map.set t.by_id ~key:order.id ~data:(order.side, order.price) in
    match order.side with
    | Side.Buy ->
      let bids =
        Map.update t.bids order.price ~f:(fun level -> enqueue_at_level level order)
      in
      { t with bids; by_id }
    | Side.Sell ->
      let asks =
        Map.update t.asks order.price ~f:(fun level -> enqueue_at_level level order)
      in
      { t with asks; by_id })
;;

let cancel t order_id =
  match Map.find t.by_id order_id with
  | None ->
    Or_error.error_s [%message "Book.cancel: unknown order" (order_id : Order_id.t)]
  | Some (side, price) ->
    let by_id = Map.remove t.by_id order_id in
    (match side with
     | Side.Buy ->
       let bids =
         match Map.find t.bids price with
         | None -> t.bids
         | Some level ->
           (match remove_from_level level order_id with
            | None -> Map.remove t.bids price
            | Some level -> Map.set t.bids ~key:price ~data:level)
       in
       Ok { t with bids; by_id }
     | Side.Sell ->
       let asks =
         match Map.find t.asks price with
         | None -> t.asks
         | Some level ->
           (match remove_from_level level order_id with
            | None -> Map.remove t.asks price
            | Some level -> Map.set t.asks ~key:price ~data:level)
       in
       Ok { t with asks; by_id })
;;

(** In-place quantity change at the same price, preserving queue position.

    Allowed when [0 < new_qty <= old_qty]. Reducing size without losing time
    priority matches a common exchange rule ("priority preserved on downsize").
    Increasing size in place would let an order keep old priority while adding
    size — we reject that; use [modify_reinsert] instead. [new_qty = 0] is
    rejected — call [cancel]. *)
let modify_in_place t order_id ~new_qty =
  if new_qty <= Quantity.zero
  then
    Or_error.error_string
      "Book.modify_in_place: new_qty must be positive (use cancel to remove)"
  else (
    match Map.find t.by_id order_id with
    | None ->
      Or_error.error_s
        [%message "Book.modify_in_place: unknown order" (order_id : Order_id.t)]
    | Some (side, price) ->
      (match find t order_id with
       | None ->
         Or_error.error_s
           [%message "Book.modify_in_place: index desync" (order_id : Order_id.t)]
       | Some order ->
         if new_qty > order.quantity
         then
           Or_error.error_s
             [%message
               "Book.modify_in_place: cannot increase quantity in place (use \
                modify_reinsert)"
                 (order_id : Order_id.t)
                 ~old_qty:(order.quantity : Quantity.t)
                 (new_qty : Quantity.t)]
         else (
           let set_qty (o : Order.t) = { o with quantity = new_qty } in
           match side with
           | Side.Buy ->
             (match Map.find t.bids price with
              | None ->
                Or_error.error_string "Book.modify_in_place: missing bid level"
              | Some level ->
                let%bind.Or_error level =
                  update_in_level level order_id ~f:set_qty
                in
                Ok { t with bids = Map.set t.bids ~key:price ~data:level })
           | Side.Sell ->
             (match Map.find t.asks price with
              | None ->
                Or_error.error_string "Book.modify_in_place: missing ask level"
              | Some level ->
                let%bind.Or_error level =
                  update_in_level level order_id ~f:set_qty
                in
                Ok { t with asks = Map.set t.asks ~key:price ~data:level }))))
;;

(** Cancel + reinsert: the order loses its old time priority.

    Use this for price changes or quantity *increases*. The new resting order is
    enqueued at the *back* of its (possibly new) price level, with whatever
    [timestamp] the caller put on [new_order]. This is the conservative FIFO
    interpretation: any material amendment is treated as a new order. *)
let modify_reinsert t (new_order : Order.t) =
  match cancel t new_order.id with
  | Error _ as err -> err
  | Ok t -> Ok (insert t new_order)
;;

(** Consume [fill_qty] from the FIFO-front resting order [maker].

    [maker] must currently be [best_order] on its side (Stage 3 matching
    maintains this). Fully filled makers are removed; partial fills downsize
    in place and keep their position at the front of the level. *)
let apply_maker_fill t (maker : Order.t) ~fill_qty =
  if fill_qty <= Quantity.zero
  then invalid_arg "Book.apply_maker_fill: fill_qty must be positive"
  else if fill_qty > maker.quantity
  then
    invalid_arg
      [%string
        "Book.apply_maker_fill: fill_qty %{Quantity.to_string fill_qty} > resting \
         %{Quantity.to_string maker.quantity}"]
  else if fill_qty = maker.quantity
  then cancel t maker.id |> Or_error.ok_exn
  else
    modify_in_place t maker.id ~new_qty:(maker.quantity - fill_qty) |> Or_error.ok_exn
;;
