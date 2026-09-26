(** Stage 1: domain types round-trip through sexp (proves ppx deriving works). *)

open Core
open Orderbook

let%expect_test "scaffold still links" =
  print_endline (status ());
  [%expect {| orderbook 0.1.0 — complete |}]
;;

let%expect_test "sample order and trade sexp" =
  let order : Order.t =
    { id = Order_id.of_int 1
    ; side = Side.Buy
    ; price = Price.of_int_exn 100
    ; quantity = Quantity.of_int_exn 5
    ; timestamp = Timestamp.of_int 42
    ; order_type = Order_type.Limit
    }
  in
  let trade : Trade.t =
    { price = Price.of_int_exn 100
    ; quantity = Quantity.of_int_exn 5
    ; timestamp = Timestamp.of_int 42
    ; aggressor_side = Side.Sell
    }
  in
  print_s [%message (order : Order.t) (trade : Trade.t)];
  [%expect
    {|
    ((order
      ((id 1) (side Buy) (price 100) (quantity 5) (timestamp 42)
       (order_type Limit)))
     (trade ((price 100) (quantity 5) (timestamp 42) (aggressor_side Sell))))
    |}]
;;

let%expect_test "side opposite" =
  print_s
    [%message
      (Side.opposite Side.Buy : Side.t) (Side.opposite Side.Sell : Side.t)];
  [%expect {| (("Side.opposite Side.Buy" Sell) ("Side.opposite Side.Sell" Buy)) |}]
;;
