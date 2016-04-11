open Core.Std

(* This test contains control chars literally in the ML source.  Keep this test separate
   from other tests in [tests.ml] because the control chars seem to provoke odd behaviour
   when running commands like [hg diff] *)

let%expect_test _ =
  let chars0_to_32() =
    let s =
      List.range 0 32
      |> List.map ~f:(fun i -> String.of_char (Char.of_int_exn i))
      (* We use a [sep] to avoid having a trailing tab char in the expected output, which
        would be hard to write below, because our editors trim trailing whitespace. *)
      |> String.concat ~sep:"x"
    in
    print_string s
  in

  chars0_to_32();
  [%expect_exact {| xxxxxxxxx	x
xxxxxxxxxxxxxxxxxxxxx|}];

  chars0_to_32();
  [%expect {|
     xxxxxxxxx	x
    xxxxxxxxxxxxxxxxxxxxx|}]
