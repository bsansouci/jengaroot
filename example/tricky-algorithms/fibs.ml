open Decrement

let rec fibs_acc n acc a b =
  if n = 0 then List.rev acc else
    fibs_acc (decrement n) (a::acc) b (a+b)

let fibs n = fibs_acc n [] 1 1

let print_ints xs = Printf.printf "result:"; List.iter (Printf.printf " %d") xs

(* Example of an inline expect test... *)

let%expect_test _ =
  print_ints (fibs 7);
  [%expect {|result: 1 1 2 3 5 8 13|}]
