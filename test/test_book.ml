(** Stage 2: example-based tests for insert / cancel / modify. *)

open Core
open Orderbook

let order
    ~(id : int)
    ~(side : Side.t)
    ~(price : int)
    ~(qty : int)
    ~(ts : int)
  : Order.t
  =
  { id = Order_id.of_int id
  ; side
  ; price = Price.of_int_exn price
  ; quantity = Quantity.of_int_exn qty
  ; timestamp = Timestamp.of_int ts
  ; order_type = Order_type.Limit
  }
;;

let ids_at book side price =
  Book.orders_at book side (Price.of_int_exn price)
  |> List.map ~f:(fun (o : Order.t) -> Order_id.to_int o.id)
;;

let%test_unit "insert sets best bid and best ask" =
  let book =
    Book.empty
    |> fun b -> Book.insert b (order ~id:1 ~side:Buy ~price:100 ~qty:5 ~ts:1)
    |> fun b -> Book.insert b (order ~id:2 ~side:Buy ~price:99 ~qty:5 ~ts:2)
    |> fun b -> Book.insert b (order ~id:3 ~side:Sell ~price:101 ~qty:5 ~ts:3)
    |> fun b -> Book.insert b (order ~id:4 ~side:Sell ~price:105 ~qty:5 ~ts:4)
  in
  [%test_result: int option]
    (Book.best_bid book |> Option.map ~f:Price.to_int)
    ~expect:(Some 100);
  [%test_result: int option]
    (Book.best_ask book |> Option.map ~f:Price.to_int)
    ~expect:(Some 101)
;;

let%test_unit "same-price level is FIFO by insert order" =
  let book =
    Book.empty
    |> fun b -> Book.insert b (order ~id:1 ~side:Buy ~price:100 ~qty:1 ~ts:1)
    |> fun b -> Book.insert b (order ~id:2 ~side:Buy ~price:100 ~qty:1 ~ts:2)
    |> fun b -> Book.insert b (order ~id:3 ~side:Buy ~price:100 ~qty:1 ~ts:3)
  in
  [%test_result: int list] (ids_at book Buy 100) ~expect:[ 1; 2; 3 ]
;;

let%test_unit "cancel removes order and drops empty price level" =
  let book =
    Book.empty
    |> fun b -> Book.insert b (order ~id:1 ~side:Sell ~price:50 ~qty:2 ~ts:1)
    |> fun b -> Book.insert b (order ~id:2 ~side:Sell ~price:50 ~qty:2 ~ts:2)
  in
  let book = Book.cancel book (Order_id.of_int 1) |> Or_error.ok_exn in
  [%test_result: int list] (ids_at book Sell 50) ~expect:[ 2 ];
  let book = Book.cancel book (Order_id.of_int 2) |> Or_error.ok_exn in
  [%test_result: int list] (ids_at book Sell 50) ~expect:[];
  [%test_result: int option]
    (Book.best_ask book |> Option.map ~f:Price.to_int)
    ~expect:None;
  [%test_result: bool] (Book.mem book (Order_id.of_int 2)) ~expect:false
;;

let%test_unit "modify_in_place downsizes and keeps queue position" =
  let book =
    Book.empty
    |> fun b -> Book.insert b (order ~id:1 ~side:Buy ~price:10 ~qty:5 ~ts:1)
    |> fun b -> Book.insert b (order ~id:2 ~side:Buy ~price:10 ~qty:5 ~ts:2)
    |> fun b -> Book.insert b (order ~id:3 ~side:Buy ~price:10 ~qty:5 ~ts:3)
  in
  let book =
    Book.modify_in_place book (Order_id.of_int 2) ~new_qty:(Quantity.of_int_exn 1)
    |> Or_error.ok_exn
  in
  [%test_result: int list] (ids_at book Buy 10) ~expect:[ 1; 2; 3 ];
  let qty2 =
    Book.find book (Order_id.of_int 2)
    |> Option.value_exn
    |> fun (o : Order.t) -> Quantity.to_int o.quantity
  in
  [%test_result: int] qty2 ~expect:1
;;

let%test_unit "modify_in_place rejects quantity increase" =
  let book =
    Book.empty |> fun b -> Book.insert b (order ~id:1 ~side:Buy ~price:10 ~qty:5 ~ts:1)
  in
  match
    Book.modify_in_place book (Order_id.of_int 1) ~new_qty:(Quantity.of_int_exn 9)
  with
  | Ok _ -> failwith "expected error on quantity increase"
  | Error _ -> ()
;;

let%test_unit "modify_reinsert loses time priority at same price" =
  let book =
    Book.empty
    |> fun b -> Book.insert b (order ~id:1 ~side:Buy ~price:10 ~qty:5 ~ts:1)
    |> fun b -> Book.insert b (order ~id:2 ~side:Buy ~price:10 ~qty:5 ~ts:2)
    |> fun b -> Book.insert b (order ~id:3 ~side:Buy ~price:10 ~qty:5 ~ts:3)
  in
  (* Amend order 1: treat as new arrival at back of the level. *)
  let amended = order ~id:1 ~side:Buy ~price:10 ~qty:8 ~ts:99 in
  let book = Book.modify_reinsert book amended |> Or_error.ok_exn in
  [%test_result: int list] (ids_at book Buy 10) ~expect:[ 2; 3; 1 ]
;;

let%expect_test "book status" =
  print_endline (status ());
  [%expect {| orderbook 0.1.0 — complete |}]
;;
