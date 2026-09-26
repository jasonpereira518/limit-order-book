(** Stage 3: matching engine example-based tests. *)

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

let fill_qtys (result : Engine.result) =
  List.map result.fills ~f:(fun (f : Fill.t) -> Quantity.to_int f.quantity)
;;

let fill_maker_ids (result : Engine.result) =
  List.map result.fills ~f:(fun (f : Fill.t) -> Order_id.to_int f.maker_id)
;;

let trade_tape (result : Engine.result) =
  List.map result.trades ~f:(fun (t : Trade.t) ->
    Price.to_int t.price, Quantity.to_int t.quantity, Side.to_string t.aggressor_side)
;;

let%test_unit "full fill against a single resting order" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:5 ~ts:1 ()) in
  let result = Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ()) in
  [%test_result: Engine.status] result.status ~expect:Accepted;
  [%test_result: int list] (fill_qtys result) ~expect:[ 5 ];
  [%test_result: int] (Quantity.to_int result.remaining_qty) ~expect:0;
  [%test_result: int option]
    (Book.best_ask result.book |> Option.map ~f:Price.to_int)
    ~expect:None;
  [%test_result: bool] (Book.is_crossed result.book) ~expect:false
;;

let%test_unit "partial fill rests the limit remainder" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:3 ~ts:1 ()) in
  let result = Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ()) in
  [%test_result: int list] (fill_qtys result) ~expect:[ 3 ];
  [%test_result: int] (Quantity.to_int result.remaining_qty) ~expect:2;
  [%test_result: int option]
    (Book.best_bid result.book |> Option.map ~f:Price.to_int)
    ~expect:(Some 100);
  [%test_result: int]
    (Book.orders_at result.book Buy (Price.of_int_exn 100)
     |> List.map ~f:(fun o -> Quantity.to_int o.quantity)
     |> List.hd_exn)
    ~expect:2
;;

let%test_unit "no fill when limit does not cross" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:105 ~qty:5 ~ts:1 ()) in
  let result = Engine.submit book (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ()) in
  [%test_result: int list] (fill_qtys result) ~expect:[];
  [%test_result: int] (Quantity.to_int result.remaining_qty) ~expect:5;
  [%test_result: int option]
    (Book.best_bid result.book |> Option.map ~f:Price.to_int)
    ~expect:(Some 100);
  [%test_result: int option]
    (Book.best_ask result.book |> Option.map ~f:Price.to_int)
    ~expect:(Some 105)
;;

let%test_unit "price-time priority: earlier order at same price fills first" =
  let book =
    Book.empty
    |> fun b -> rest b (mk ~id:1 ~side:Sell ~price:100 ~qty:2 ~ts:1 ())
    |> fun b -> rest b (mk ~id:2 ~side:Sell ~price:100 ~qty:2 ~ts:2 ())
  in
  let result = Engine.submit book (mk ~id:3 ~side:Buy ~price:100 ~qty:3 ~ts:3 ()) in
  [%test_result: int list] (fill_maker_ids result) ~expect:[ 1; 2 ];
  [%test_result: int list] (fill_qtys result) ~expect:[ 2; 1 ];
  (* Order 2 keeps 1 residual at the front of 100. *)
  [%test_result: (int * int) list]
    (Book.orders_at result.book Sell (Price.of_int_exn 100)
     |> List.map ~f:(fun o -> Order_id.to_int o.id, Quantity.to_int o.quantity))
    ~expect:[ 2, 1 ]
;;

let%test_unit "FOK rejects without mutating the book when unfillable" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:3 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ~order_type:Fok ())
  in
  [%test_result: Engine.status] result.status ~expect:(Rejected Fok_unfillable);
  [%test_result: int list] (fill_qtys result) ~expect:[];
  [%test_result: int option]
    (Book.best_ask result.book |> Option.map ~f:Price.to_int)
    ~expect:(Some 100);
  [%test_result: int]
    (Book.find result.book (Order_id.of_int 1)
     |> Option.value_exn
     |> fun (o : Order.t) -> Quantity.to_int o.quantity)
    ~expect:3
;;

let%test_unit "FOK commits when fully fillable across two levels" =
  let book =
    Book.empty
    |> fun b -> rest b (mk ~id:1 ~side:Sell ~price:100 ~qty:2 ~ts:1 ())
    |> fun b -> rest b (mk ~id:2 ~side:Sell ~price:101 ~qty:3 ~ts:2 ())
  in
  let result =
    Engine.submit
      book
      (mk ~id:3 ~side:Buy ~price:101 ~qty:5 ~ts:3 ~order_type:Fok ())
  in
  [%test_result: Engine.status] result.status ~expect:Accepted;
  [%test_result: int list] (fill_qtys result) ~expect:[ 2; 3 ];
  [%test_result: bool] (Book.mem result.book (Order_id.of_int 1)) ~expect:false;
  [%test_result: bool] (Book.mem result.book (Order_id.of_int 2)) ~expect:false
;;

let%test_unit "post-only rejects when it would immediately take" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:5 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:100 ~qty:1 ~ts:2 ~order_type:Post_only ())
  in
  [%test_result: Engine.status] result.status ~expect:(Rejected Post_only_would_take);
  [%test_result: bool] (Book.mem result.book (Order_id.of_int 2)) ~expect:false;
  [%test_result: int option]
    (Book.best_ask result.book |> Option.map ~f:Price.to_int)
    ~expect:(Some 100)
;;

let%test_unit "post-only rests when it does not cross" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:105 ~qty:5 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:100 ~qty:1 ~ts:2 ~order_type:Post_only ())
  in
  [%test_result: Engine.status] result.status ~expect:Accepted;
  [%test_result: int list] (fill_qtys result) ~expect:[];
  [%test_result: bool] (Book.mem result.book (Order_id.of_int 2)) ~expect:true
;;

let%test_unit "market never rests; partial fill leaves no bid" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:2 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:0 ~qty:5 ~ts:2 ~order_type:Market ())
  in
  [%test_result: int list] (fill_qtys result) ~expect:[ 2 ];
  [%test_result: int] (Quantity.to_int result.remaining_qty) ~expect:3;
  [%test_result: int option]
    (Book.best_bid result.book |> Option.map ~f:Price.to_int)
    ~expect:None
;;

let%test_unit "IOC cancels unfilled remainder instead of resting" =
  let book = rest Book.empty (mk ~id:1 ~side:Sell ~price:100 ~qty:2 ~ts:1 ()) in
  let result =
    Engine.submit
      book
      (mk ~id:2 ~side:Buy ~price:100 ~qty:5 ~ts:2 ~order_type:Ioc ())
  in
  [%test_result: int list] (fill_qtys result) ~expect:[ 2 ];
  [%test_result: int] (Quantity.to_int result.remaining_qty) ~expect:3;
  [%test_result: int option]
    (Book.best_bid result.book |> Option.map ~f:Price.to_int)
    ~expect:None
;;

let%test_unit "public tape records aggressor side and maker price" =
  let book =
    Book.empty
    |> fun b -> rest b (mk ~id:1 ~side:Buy ~price:99 ~qty:2 ~ts:1 ())
    |> fun b -> rest b (mk ~id:2 ~side:Buy ~price:98 ~qty:2 ~ts:2 ())
  in
  let result =
    Engine.submit book (mk ~id:3 ~side:Sell ~price:98 ~qty:3 ~ts:3 ())
  in
  [%test_result: (int * int * string) list]
    (trade_tape result)
    ~expect:[ 99, 2, "Sell"; 98, 1, "Sell" ]
;;

let%expect_test "engine status" =
  print_endline (status ());
  [%expect {| orderbook 0.1.0 — complete |}]
;;
