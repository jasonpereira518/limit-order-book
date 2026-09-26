(** Core domain types for the limit order book.

    These are pure data definitions — no book or matching logic yet. Keeping them
    in one module makes the vocabulary of the system easy to learn and keeps
    later stages (book, matching, tests) speaking the same language. *)

open Core

(** Which side of the market an order sits on.

    - [Buy]: a bid — willingness to purchase at a given price (or better).
    - [Sell]: an ask/offer — willingness to sell at a given price (or better). *)
module Side = struct
  type t =
    | Buy
    | Sell
  [@@deriving sexp, compare, equal, hash]

  let opposite = function
    | Buy -> Sell
    | Sell -> Buy
  ;;

  let to_string = function
    | Buy -> "Buy"
    | Sell -> "Sell"
  ;;
end

(** Opaque-ish identifier for a single order.

    Distinct from [Price.t] / [Quantity.t] so you cannot accidentally pass a
    price where an id is expected. Assigned by the engine (or simulator), not
    by the exchange client in this project. *)
module Order_id = struct
  type t = int [@@deriving sexp, compare, equal, hash]

  let of_int id = id
  let to_int id = id
  let to_string id = Int.to_string id
end

(** Price in integer ticks (e.g. cents), never [float].

    Why not float? Matching depends on exact equality and total ordering
    ("is this bid still at or above the ask?"). IEEE floats make those
    questions unreliable ([0.1 +. 0.2 <> 0.3], NaNs break ordering). Exchanges
    therefore represent prices as integers in the instrument's minimum tick
    size. Here [1] means one tick; the mapping tick→dollars is a display
    concern, not a matching concern. *)
module Price = struct
  type t = int [@@deriving sexp, compare, equal, hash]

  let of_int_exn ticks =
    if ticks < 0
    then invalid_arg [%string "Price.of_int_exn: expected non-negative, got %{ticks#Int}"]
    else ticks
  ;;

  let to_int t = t
  let zero = 0
  let to_string t = Int.to_string t
end

(** How many units (shares, contracts, …) an order wants to trade.

    Also an [int], for the same exactness reasons as [Price]. Must be
    positive for live orders; zero appears only as a "fully filled / gone"
    sentinel in engine logic later. *)
module Quantity = struct
  type t = int [@@deriving sexp, compare, equal, hash]

  let of_int_exn qty =
    if qty < 0
    then
      invalid_arg
        [%string "Quantity.of_int_exn: expected non-negative, got %{qty#Int}"]
    else qty
  ;;

  let to_int t = t
  let zero = 0
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( >= ) = ( >= )
  let ( > ) = ( > )
  let ( < ) = ( < )
  let min = Int.min
  let to_string t = Int.to_string t
end

(** Logical arrival time used for price-time (FIFO) priority.

    This is a monotonic counter assigned by the engine when an order is
    accepted — not a wall-clock timestamp. Wall-clock time is useful for a
    public tape display, but FIFO priority must be deterministic and total;
    a counter gives us that without depending on clock resolution. Stage 6's
    simulator can still record wall-clock separately for latency measurement. *)
module Timestamp = struct
  type t = int [@@deriving sexp, compare, equal, hash]

  let of_int t = t
  let to_int t = t
  let zero = 0
  let to_string t = Int.to_string t
end

(** How an order should behave when it interacts with the book.

    - [Limit]: rest on the book at [price] if not fully filled.
    - [Market]: take liquidity immediately; never rest (may partially fill).
    - [Ioc] (immediate-or-cancel): fill what you can now, cancel the rest;
      never rest.
    - [Fok] (fill-or-kill): all-or-nothing — fully fill now, or reject with
      no change to the book.
    - [Post_only]: only accept if the order would rest (provide liquidity);
      reject if it would immediately cross and take liquidity. *)
module Order_type = struct
  type t =
    | Limit
    | Market
    | Ioc
    | Fok
    | Post_only
  [@@deriving sexp, compare, equal, hash]

  let to_string = function
    | Limit -> "Limit"
    | Market -> "Market"
    | Ioc -> "IOC"
    | Fok -> "FOK"
    | Post_only -> "PostOnly"
  ;;
end

(** A single order: the unit of intent submitted to the matching engine.

    [price] is ignored for [Market] orders at matching time (they walk the
    opposite side regardless of limit), but we still store a field so the
    record shape is uniform — Stage 3 will document the exact semantics. *)
module Order = struct
  type t =
    { id : Order_id.t
    ; side : Side.t
    ; price : Price.t
    ; quantity : Quantity.t
    ; timestamp : Timestamp.t
    ; order_type : Order_type.t
    }
  [@@deriving sexp, compare, equal]
end

(** Engine-internal record of one match between a resting (maker) order and
    an incoming (taker/aggressor) order.

    Fills are the detailed audit trail: who traded with whom. The public
    tape usually does not show both ids; see [Trade] for that view. *)
module Fill = struct
  type t =
    { maker_id : Order_id.t
    ; taker_id : Order_id.t
    ; price : Price.t
    ; quantity : Quantity.t
    ; timestamp : Timestamp.t
    }
  [@@deriving sexp, compare, equal]
end

(** Public trade-tape record: what the market observes after a match.

    [aggressor_side] is the side of the incoming order that initiated the
    trade (the taker). Price is the resting order's price under standard
    price-time matching (the maker's quote). *)
module Trade = struct
  type t =
    { price : Price.t
    ; quantity : Quantity.t
    ; timestamp : Timestamp.t
    ; aggressor_side : Side.t
    }
  [@@deriving sexp, compare, equal]
end
