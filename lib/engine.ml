(** Matching engine: submit an incoming order against resting liquidity.

    Price-time priority
    - Walk the opposite side from best price to worse.
    - Within a price level, fill FIFO (front of [Fqueue] first).
    - Trade price is the *resting* (maker) price.

    Order-type summary
    - [Limit]: match, then rest any remainder on the book.
    - [Market]: match as much as possible; never rest (remainder is cancelled).
    - [Ioc]: same as market w.r.t. resting — fill now, cancel the rest. Unlike
      market, still respects the limit price (will not walk through levels
      beyond [order.price]).
    - [Fok]: all-or-nothing. Because [Book.t] is persistent, we try a full match
      and *discard* the candidate book if anything would remain unfilled.
    - [Post_only]: reject if the order would immediately take liquidity;
      otherwise rest it with no fills.

    After a successful accept path, the book must not be crossed
    ([Book.is_crossed] = false). *)

open Core
open Types

type reject_reason =
  | Fok_unfillable
  | Post_only_would_take
  | Duplicate_order_id
  | Non_positive_quantity
[@@deriving sexp, compare, equal]

type status =
  | Accepted
  | Rejected of reject_reason
[@@deriving sexp, compare, equal]

type result =
  { book : Book.t
  ; fills : Fill.t list
  ; trades : Trade.t list
  ; status : status
  ; (** Qty still unfilled after the match attempt (0 if fully filled).
        For [Accepted] limit orders this remainder was rested; for market/IOC
        it was cancelled; for FOK/post-only rejects it is the original qty. *)
    remaining_qty : Quantity.t
  }
[@@deriving sexp_of]

let would_take_liquidity (book : Book.t) (order : Order.t) =
  match order.side with
  | Side.Buy ->
    (match Book.best_ask book with
     | None -> false
     | Some ask -> Price.compare order.price ask >= 0)
  | Side.Sell ->
    (match Book.best_bid book with
     | None -> false
     | Some bid -> Price.compare order.price bid <= 0)
;;

(** Can this aggressor trade against a resting order at [resting_price]? *)
let price_eligible (order : Order.t) ~resting_price =
  match order.order_type with
  | Order_type.Market -> true
  | Limit | Ioc | Fok | Post_only ->
    (match order.side with
     | Side.Buy -> Price.compare order.price resting_price >= 0
     | Side.Sell -> Price.compare order.price resting_price <= 0)
;;

let make_fill ~(maker : Order.t) ~(taker : Order.t) ~qty =
  let fill
      : Fill.t =
    { maker_id = maker.id
    ; taker_id = taker.id
    ; price = maker.price
    ; quantity = qty
    ; timestamp = taker.timestamp
    }
  in
  let trade
      : Trade.t =
    { price = maker.price
    ; quantity = qty
    ; timestamp = taker.timestamp
    ; aggressor_side = taker.side
    }
  in
  fill, trade
;;

(** Walk the opposite side, generating fills until the aggressor is done or no
    eligible liquidity remains. Returns the updated book, fills/trades in
    chronological order, and the aggressor with reduced [quantity]. *)
let match_aggressive (book : Book.t) (aggressor : Order.t) =
  let opposite = Side.opposite aggressor.side in
  let rec loop book (aggressor : Order.t) fills trades =
    if aggressor.quantity <= Quantity.zero
    then book, List.rev fills, List.rev trades, aggressor
    else (
      match Book.best_order book opposite with
      | None -> book, List.rev fills, List.rev trades, aggressor
      | Some maker ->
        if not (price_eligible aggressor ~resting_price:maker.price)
        then book, List.rev fills, List.rev trades, aggressor
        else (
          let qty = Quantity.min aggressor.quantity maker.quantity in
          let fill, trade = make_fill ~maker ~taker:aggressor ~qty in
          let book = Book.apply_maker_fill book maker ~fill_qty:qty in
          let aggressor = { aggressor with quantity = aggressor.quantity - qty } in
          loop book aggressor (fill :: fills) (trade :: trades)))
  in
  loop book aggressor [] []
;;

let accept book fills trades remaining_qty =
  { book; fills; trades; status = Accepted; remaining_qty }
;;

let reject book reason ~(original_qty : Quantity.t) =
  { book
  ; fills = []
  ; trades = []
  ; status = Rejected reason
  ; remaining_qty = original_qty
  }
;;

let submit (book : Book.t) (order : Order.t) : result =
  if order.quantity <= Quantity.zero
  then reject book Non_positive_quantity ~original_qty:order.quantity
  else if Book.mem book order.id
  then reject book Duplicate_order_id ~original_qty:order.quantity
  else (
    match order.order_type with
    | Order_type.Post_only ->
      if would_take_liquidity book order
      then reject book Post_only_would_take ~original_qty:order.quantity
      else (
        let book = Book.insert book order in
        accept book [] [] Quantity.zero)
    | Fok ->
      let candidate, fills, trades, leftover = match_aggressive book order in
      if leftover.quantity > Quantity.zero
      then reject book Fok_unfillable ~original_qty:order.quantity
      else accept candidate fills trades Quantity.zero
    | Market ->
      let book, fills, trades, leftover = match_aggressive book order in
      (* Market never rests — leftover is cancelled. *)
      accept book fills trades leftover.quantity
    | Ioc ->
      let book, fills, trades, leftover = match_aggressive book order in
      accept book fills trades leftover.quantity
    | Limit ->
      let book, fills, trades, leftover = match_aggressive book order in
      if leftover.quantity > Quantity.zero
      then (
        let book = Book.insert book leftover in
        accept book fills trades leftover.quantity)
      else accept book fills trades Quantity.zero)
;;
