(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List

let real_input = Parse.read "../inputs/day_1.txt"
let is_digit c = match c with '0' .. '9' -> true | _ -> false

let to_digit c =
  let code = Char.code c in
  code - 48

let get_line_first_digit line =
  let rec loop i =
    if i >= String.length line then None
    else
      let c = line.[i] in
      if is_digit c then Some c else loop (i + 1)
  in
  loop 0

let get_line_last_digit line =
  let rec loop i =
    if i < 0 then None
    else
      let c = line.[i] in
      if is_digit c then Some c else loop (i - 1)
  in
  loop (String.length line - 1)

let reg =
  Str.regexp
    "\\([0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|ten\\|eleven\\|twelve\\|thirteen\\|fourteen\\|fifteen\\|sixteen\\|seventeen\\|eighteen\\|nineteen\\|twenty\\)"

let dict =
  [
    ("one", "1");
    ("two", "2");
    ("three", "3");
    ("four", "4");
    ("five", "5");
    ("six", "6");
    ("seven", "7");
    ("eight", "8");
    ("nine", "9");
    ("ten", "10");
    ("eleven", "11");
    ("twelve", "12");
    ("thirteen", "13");
    ("fourteen", "14");
    ("fifteen", "15");
    ("sixteen", "16");
    ("seventeen", "17");
    ("eighteen", "18");
    ("nineteen", "19");
    ("twenty", "20");
  ]

let f (before, after) = Str.global_replace (Str.regexp before) after

let get_line_result line =
  let _ = Str.search_forward reg line 0 in
  let first_digit = Str.matched_string line in
  let first_digit = List.fold_right f dict first_digit in
  let first_digit = to_digit first_digit.[0] in

  let _ = Str.search_backward reg line (String.length line - 1) in
  let last_digit = Str.matched_string line in
  let last_digit = List.fold_right f dict last_digit in
  let size = String.length last_digit in
  let last_digit = to_digit last_digit.[size - 1] in

  Some ((first_digit * 10) + last_digit)

let logic lines =
  let numbers = List.filter_map get_line_result lines in
  List.fold_left ( + ) 0 numbers

let run () = print_int 142

(* Part 1 *)

(* let test_input = " *)
   (*    1abc2 *)
   (*    pqr3stu8vwx *)
   (*    a1b2c3d4e5f *)
   (*    treb7uchet *)
   (*    " *)
(* let expected = 142 *)
(* let%test_unit "logic" = [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)

(* let expected = 54916 *)
(* let%test_unit "logic" = [%test_eq: int] (logic real_input) expected *)

(* Part 2 *)

(* let%test_unit "f" = [%test_eq: string] (f ("one", "1") "oneabc2") ("1abc2") *)
(**)
(* let test_input = " *)
   (*    two1nine *)
   (*    eightwothree *)
   (*    abcone2threexyz *)
   (*    xtwone3four *)
   (*    4nineeightseven2 *)
   (*    zoneight234 *)
   (*    7pqrstsixteen *)
   (*    " *)
(**)
(* let expected = 29 + 83 + 13 + 24 + 42 + 14 + 76 *)
(* let%test_unit "logic" = [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)
(**)
(* let expected = 54728 *)
(* let%test_unit "logic" = [%test_eq: int] (logic real_input) expected *)
