open Tricky_algorithms

let printf = Printf.printf

let go n =
  printf "fibs (fact %d) ->" n;
  List.iter (printf " %d") (Fibs.fibs (Fact.fact n));
  printf "\n"

let main = go (int_of_string (Sys.argv.(1)))

