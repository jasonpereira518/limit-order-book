(** Pedagogical shrink demo: a *deliberate* LIFO matcher violates time priority,
    and Quickcheck shrinks the failing sequence to a minimal counterexample.

    Production code is untouched — the bug lives only in this test file. *)

open Core
open Orderbook
open Market_events

(** Fill the *last* order at the best opposite price (LIFO), not the first (FIFO). *)
let rec match_lifo (book : Book.t) (aggressor : Order.t) fills =
  if aggressor.quantity <= Quantity.zero
  then book, List.rev fills, aggressor
  else (
    match Book.best_bid book, Book.best_ask book, aggressor.side with
    | _, Some ask, Buy when Price.compare aggressor.price ask >= 0 ->
      (match Book.orders_at book Sell ask |> List.rev with
       | [] -> book, List.rev fills, aggressor
       | maker :: _ ->
         let qty = Quantity.min aggressor.quantity maker.quantity in
         let fill
             : Fill.t =
           { maker_id = maker.id
           ; taker_id = aggressor.id
           ; price = maker.price
           ; quantity = qty
           ; timestamp = aggressor.timestamp
           }
         in
         let book = Book.apply_maker_fill book maker ~fill_qty:qty in
         let aggressor = { aggressor with quantity = aggressor.quantity - qty } in
         match_lifo book aggressor (fill :: fills))
    | Some bid, _, Sell when Price.compare aggressor.price bid <= 0 ->
      (match Book.orders_at book Buy bid |> List.rev with
       | [] -> book, List.rev fills, aggressor
       | maker :: _ ->
         let qty = Quantity.min aggressor.quantity maker.quantity in
         let fill
             : Fill.t =
           { maker_id = maker.id
           ; taker_id = aggressor.id
           ; price = maker.price
           ; quantity = qty
           ; timestamp = aggressor.timestamp
           }
         in
         let book = Book.apply_maker_fill book maker ~fill_qty:qty in
         let aggressor = { aggressor with quantity = aggressor.quantity - qty } in
         match_lifo book aggressor (fill :: fills))
    | _ -> book, List.rev fills, aggressor)
;;

let submit_lifo (book : Book.t) (order : Order.t) =
  let book, fills, leftover = match_lifo book order [] in
  if leftover.quantity > Quantity.zero
  then Book.insert book leftover, fills
  else book, fills
;;

(** Replay only limit submits (enough to expose LIFO), checking that each
    aggressor’s *first* fill is the FIFO front — not merely that timestamps are
    sorted (a single wrong-maker fill would pass a sort check). *)
let check_lifo_priority (actions : Action.t list) =
  let book = ref Book.empty in
  let next_id = ref 1 in
  let next_ts = ref 1 in
  List.iter actions ~f:(function
    | Cancel _ | Modify_down _ -> ()
    | Submit { side; order_type; price; qty } ->
      (match order_type with
       | Limit ->
         let id = !next_id in
         let ts = !next_ts in
         incr next_id;
         incr next_ts;
         let order
             : Order.t =
           { id = Order_id.of_int id
           ; side
           ; price = Price.of_int_exn price
           ; quantity = Quantity.of_int_exn qty
           ; timestamp = Timestamp.of_int ts
           ; order_type = Limit
           }
         in
         let expected_front = Book.best_order !book (Side.opposite side) in
         let book', fills = submit_lifo !book order in
         book := book';
         (match fills, expected_front with
          | [], _ -> ()
          | (fill : Fill.t) :: _, None ->
            failwith
              (Sexp.to_string_hum
                 [%message "filled against empty opposite side" (fill : Fill.t)])
          | (fill : Fill.t) :: _, Some (front : Order.t) ->
            if not (Order_id.equal fill.maker_id front.id)
            then
              failwith
                (Sexp.to_string_hum
                   [%message
                     "LIFO bug: skipped FIFO front"
                       (actions : Action.t list)
                       ~expected_maker:(Order_id.to_int front.id : int)
                       ~actual_first_maker:(Order_id.to_int fill.maker_id : int)
                       (fills : Fill.t list)]))
       | _ -> ()))
;;

let gen_limit_actions : Action.t list Quickcheck.Generator.t =
  let open Quickcheck.Generator.Let_syntax in
  let gen_limit =
    let%map side = gen_side
    and price = Int.gen_incl 100 100
    and qty = Int.gen_incl 1 2 in
    Action.Submit { side; order_type = Limit; price; qty }
  in
  let%bind length = Int.gen_incl 3 10 in
  Quickcheck.Generator.list_with_length length gen_limit
;;

let classic_lifo_trigger : Action.t list =
  [ Submit { side = Buy; order_type = Limit; price = 100; qty = 1 }
  ; Submit { side = Buy; order_type = Limit; price = 100; qty = 1 }
  ; Submit { side = Sell; order_type = Limit; price = 100; qty = 1 }
  ]
;;

let%expect_test "shrinking finds a minimal LIFO counterexample" =
  (match
     Quickcheck.test_or_error
       gen_limit_actions
       ~seed:(`Deterministic "stage4-lifo-shrink")
       ~sexp_of:[%sexp_of: Action.t list]
       ~shrinker:actions_shrinker
       ~shrink_attempts:`Exhaustive
       ~examples:[ classic_lifo_trigger ]
       ~trials:100
       ~f:(fun actions ->
         try
           check_lifo_priority actions;
           Ok ()
         with
         | exn -> Or_error.of_exn exn)
   with
   | Ok () -> print_endline "UNEXPECTED: LIFO bug was not detected"
   | Error err -> print_s [%sexp (err : Error.t)]);
  [%expect {|
    ("Base_quickcheck.Test.run: test failed"
     (input
      ((Submit (side Buy) (order_type Limit) (price 100) (qty 1))
       (Submit (side Buy) (order_type Limit) (price 100) (qty 1))
       (Submit (side Sell) (order_type Limit) (price 100) (qty 1))))
     (error
      (Failure
        "(\"LIFO bug: skipped FIFO front\"\
       \n (actions\
       \n  ((Submit (side Buy) (order_type Limit) (price 100) (qty 1))\
       \n   (Submit (side Buy) (order_type Limit) (price 100) (qty 1))\
       \n   (Submit (side Sell) (order_type Limit) (price 100) (qty 1))))\
       \n (expected_maker 1) (actual_first_maker 2)\
       \n (fills (((maker_id 2) (taker_id 3) (price 100) (quantity 1) (timestamp 3)))))")))
    |}]
;;
