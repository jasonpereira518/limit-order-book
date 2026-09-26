(** Stage 4: property-based invariant tests over random event sequences. *)

open Core
open Market_events

let%test_unit "random sequences: no cross, qty conserved, price-time priority" =
  Quickcheck.test
    gen_actions
    ~sexp_of:[%sexp_of: Action.t list]
    ~shrinker:actions_shrinker
    ~trials:300
    ~f:check_actions
;;

let%test_unit "hand-written crossing pressure still conserves" =
  (* Deterministic warm-up: build both sides, then fire markets through the
     spread so fills, cancels, and rests all interact. *)
  check_actions
    [ Submit { side = Buy; order_type = Limit; price = 100; qty = 5 }
    ; Submit { side = Buy; order_type = Limit; price = 99; qty = 5 }
    ; Submit { side = Sell; order_type = Limit; price = 101; qty = 5 }
    ; Submit { side = Sell; order_type = Limit; price = 102; qty = 5 }
    ; Submit { side = Buy; order_type = Market; price = 0; qty = 7 }
    ; Submit { side = Sell; order_type = Ioc; price = 99; qty = 10 }
    ; Cancel { slot = 0 }
    ; Submit { side = Buy; order_type = Post_only; price = 98; qty = 3 }
    ; Submit { side = Buy; order_type = Fok; price = 102; qty = 100 }
    ]
;;
