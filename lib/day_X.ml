(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List
(* open Str *)
(* open Utilities *)

(* ============== TYPES ============== *)

(* ============== INPUT ============== *)

let test_input = Parse.get_lines "blablabla"

(* ============== LOGIC DAY 1 ============== *)

let logic1 (_input : string t) : int = 0

(* ============== LOGIC DAY 2 ============== *)

(* ============== RUN ============== *)

let run (_path : string) =
  (* let input = Parse.read path |> String.concat "\n" in *)
  let input = test_input in
  let result = logic1 input in
  Printf.printf "%d\n" result

(* ============== TESTS DAY 1 ============== *)

(* let%test_unit "logic1" = *)
(*   let expected = 0 in *)
(*   [%test_eq: int] (logic1 test_input) expected *)
(**)
(* let%test_unit "logic1" = *)
(*   let real_input = Parse.read "../inputs/day_20.txt" in *)
(*   let expected = 0 in *)
(*   [%test_eq: int] (logic1 real_input) expected *)

(* ============== LOGIC DAY 2 ============== *)
