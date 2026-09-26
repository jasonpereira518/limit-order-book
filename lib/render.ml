(** Human-readable snapshots of book state and the public trade tape.

    Used by expect tests (Stage 5) so failures read like a market blotter, not a
    raw sexp dump. Keep the format stable — expect tests pin it. *)

open Core
open Types

let order_cell (o : Order.t) =
  sprintf
    "[id=%s qty=%s ts=%s]"
    (Order_id.to_string o.id)
    (Quantity.to_string o.quantity)
    (Timestamp.to_string o.timestamp)
;;

let level_line ~price orders =
  let cells = List.map orders ~f:order_cell |> String.concat ~sep:" " in
  sprintf "  %s: %s" (Price.to_string price) cells
;;

let show_book (book : Book.t) =
  let bids =
    match Book.bid_levels book with
    | [] -> [ "  (empty)" ]
    | levels -> List.map levels ~f:(fun (price, orders) -> level_line ~price orders)
  in
  let asks =
    match Book.ask_levels book with
    | [] -> [ "  (empty)" ]
    | levels -> List.map levels ~f:(fun (price, orders) -> level_line ~price orders)
  in
  let best =
    sprintf
      "best bid=%s  best ask=%s  crossed=%b"
      (Option.value_map (Book.best_bid book) ~default:"-" ~f:Price.to_string)
      (Option.value_map (Book.best_ask book) ~default:"-" ~f:Price.to_string)
      (Book.is_crossed book)
  in
  String.concat
    ~sep:"\n"
    (List.concat
       [ [ best; "BIDS (best → worse):" ]
       ; bids
       ; [ "ASKS (best → worse):" ]
       ; asks
       ])
;;

let show_trade (t : Trade.t) =
  sprintf
    "  %s x %s  aggressor=%s  ts=%s"
    (Price.to_string t.price)
    (Quantity.to_string t.quantity)
    (Side.to_string t.aggressor_side)
    (Timestamp.to_string t.timestamp)
;;

let show_tape (trades : Trade.t list) =
  match trades with
  | [] -> "TAPE:\n  (empty)"
  | trades ->
    String.concat ~sep:"\n" ("TAPE:" :: List.map trades ~f:show_trade)
;;

let show_status (status : Engine.status) =
  match status with
  | Accepted -> "Accepted"
  | Rejected reason ->
    sprintf "Rejected %s" (Sexp.to_string_hum [%sexp (reason : Engine.reject_reason)])
;;

let show_result (result : Engine.result) =
  String.concat
    ~sep:"\n"
    [ sprintf
        "status=%s  remaining_qty=%s"
        (show_status result.status)
        (Quantity.to_string result.remaining_qty)
    ; show_tape result.trades
    ; show_book result.book
    ]
;;
