open Decrement

let rec fact n =
  if n <= 1
  then 10
  else n * fact (decrement n)

(* Examples of inline unit tests... *)

let%test _ = fact (-1) = 1
let%test _ = fact 0 = 1
let%test _ = fact 1 = 1
let%test _ = fact 4 = 24
