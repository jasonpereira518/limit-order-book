(** Stage 5: expect/snapshot tests for canonical matching scenarios.

    These pin a human-readable blotter of book + tape. When intentional behavior
    changes, update goldens with:

      dune runtest --auto-promote

    See the Stage 5 write-up for why Jane Street leans on this idiom. *)

open Core
open Orderbook

let mk
    ~(id : int)
    ~(side : Side.t)
    ~(price : int)
    ~(qty : int)
    ~(ts : int)
    ?(order_type : Order_type.t = Limit)
    ()
  : Order.t
  =
  { id = Order_id.of_int id
  ; side
  ; price = Price.of_int_exn price
  ; quantity = Quantity.of_int_exn qty
  ; timestamp = Timestamp.of_int ts
  ; order_type
  }
;;

let rest book order = Book.insert book order

let%expect_test "market order sweeps two ask levels" =
  (* Three asks at increasing prices; market buy of 7 takes 100×3 + 101×4 and
     leaves the 102 level untouched. *)
  let book =
    Book.empty
    |> fun b -> rest b (mk ~id:1 ~side:Sell ~price:100 ~qty:3 ~ts:1 ())
    |> fun b -> rest b (mk ~id:2 ~side:Sell ~price:101 ~qty:4 ~ts:2 ())
    |> fun b -> rest b (mk ~id:3 ~side:Sell ~price:102 ~qty:5 ~ts:3 ())
  in
  let result =
    Engine.submit book (mk ~id:10 ~side:Buy ~price:0 ~qty:7 ~ts:10 ~order_type:Market ())
  in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Accepted  remaining_qty=0
    TAPE:
      100 x 3  aggressor=Buy  ts=10
      101 x 4  aggressor=Buy  ts=10
    best bid=-  best ask=102  crossed=false
    BIDS (best → worse):
      (empty)
    ASKS (best → worse):
      102: [id=3 qty=5 ts=3]
    |}]
;;

let%expect_test "FIFO at one price: earlier resting order fills first" =
  let book =
    Book.empty
    |> fun b -> rest b (mk ~id:1 ~side:Buy ~price:50 ~qty:2 ~ts:1 ())
    |> fun b -> rest b (mk ~id:2 ~side:Buy ~price:50 ~qty:2 ~ts:2 ())
  in
  let result = Engine.submit book (mk ~id:3 ~side:Sell ~price:50 ~qty:3 ~ts:3 ()) in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Accepted  remaining_qty=0
    TAPE:
      50 x 2  aggressor=Sell  ts=3
      50 x 1  aggressor=Sell  ts=3
    best bid=50  best ask=-  crossed=false
    BIDS (best → worse):
      50: [id=2 qty=1 ts=2]
    ASKS (best → worse):
      (empty)
    |}]
;;

let%expect_test "partial limit fill rests the remainder" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:3 ~ts:1 ()) in
  let result = Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ()) in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Accepted  remaining_qty=2
    TAPE:
      100 x 3  aggressor=Buy  ts=2
    best bid=100  best ask=-  crossed=false
    BIDS (best → worse):
      100: [id=2 qty=2 ts=2]
    ASKS (best → worse):
      (empty)
    |}]
;;

let%expect_test "post-only that would take is rejected; book unchanged" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:5 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:100 ~qty:1 ~ts:2 ~order_type:Post_only ())
  in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Rejected Post_only_would_take  remaining_qty=1
    TAPE:
      (empty)
    best bid=-  best ask=100  crossed=false
    BIDS (best → worse):
      (empty)
    ASKS (best → worse):
      100: [id=1 qty=5 ts=1]
    |}]
;;

let%expect_test "FOK that cannot fully fill is rejected; book unchanged" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:3 ~ts:1 ()) in
  let result =
    Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ~order_type:Fok ())
  in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Rejected Fok_unfillable  remaining_qty=5
    TAPE:
      (empty)
    best bid=-  best ask=100  crossed=false
    BIDS (best → worse):
      (empty)
    ASKS (best → worse):
      100: [id=1 qty=3 ts=1]
    |}]
;;

let%expect_test "IOC cancels remainder instead of resting" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:2 ~ts:1 ()) in
  let result =
    Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ~order_type:Ioc ())
  in
  print_endline (Render.show_result result);
  [%expect
    {|
    status=Accepted  remaining_qty=3
    TAPE:
      100 x 2  aggressor=Buy  ts=2
    best bid=-  best ask=-  crossed=false
    BIDS (best → worse):
      (empty)
    ASKS (best → worse):
      (empty)
    |}]
;;
