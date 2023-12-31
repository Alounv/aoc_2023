(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List

let r_symbols = Str.regexp "[^0-9.]"
let r_star = Str.regexp "[*]"

let get_symbol_position (line : string) (reg : Str.regexp) : int list =
  let size = String.length line in

  let rec search (index : int) (acc : int list) : int list =
    if index = size then acc
    else
      try
        let new_index = Str.search_forward reg line index in
        search (new_index + 1) (new_index :: acc)
      with Not_found -> acc
  in
  let result = search 0 [] in
  result

let get_all_symbol_positions (lines : string list) (reg : Str.regexp) :
    (int * int) list =
  let rec get_positions (lines : string list) (line_index : int)
      (acc : (int * int) list) : (int * int) list =
    match lines with
    | [] -> acc
    | line :: rest ->
        let positions = get_symbol_position line reg in
        let new_acc =
          List.fold_left (fun acc pos -> (pos, line_index) :: acc) acc positions
        in
        get_positions rest (line_index + 1) new_acc
  in
  get_positions lines 0 []

let num_reg = Str.regexp "\\b[0-9]+\\b"

type num = { s : int; e : int; y : int; value : int }

let get_numbers_positions (line : string) (y : int) : num list =
  let size = String.length line in

  let rec search (index : int) (acc : num list) : num list =
    if index = size then acc
    else
      try
        let new_index = Str.search_forward num_reg line index in
        let number = Str.matched_string line in

        let new_number =
          {
            s = new_index;
            e = new_index + String.length number - 1;
            y;
            value = int_of_string number;
          }
        in

        search (new_index + 1) (new_number :: acc)
      with Not_found -> acc
  in
  let result = search 0 [] in
  result

let get_all_numbers_positions (lines : string list) : num list =
  let rec get_positions (lines : string list) (line_index : int)
      (acc : num list) : num list =
    match lines with
    | [] -> acc
    | line :: rest ->
        let positions = get_numbers_positions line line_index in
        let new_acc =
          List.fold_left (fun acc pos -> pos :: acc) acc positions
        in
        get_positions rest (line_index + 1) new_acc
  in
  get_positions lines 0 []

let is_number_touching_symbol (number : num) (symbol : int * int) : bool =
  let x, ys = symbol in
  let { s; e; y; _ } = number in
  let is_on_adjacent_line = ys - 1 <= y && y <= ys + 1 in
  if not is_on_adjacent_line then false else s - 1 <= x && x <= e + 1

let is_numer_touching_any_symbol (symbols : (int * int) list) (number : num) :
    bool =
  List.exists (fun symbol -> is_number_touching_symbol number symbol) symbols

let get_gear_power (numbers : num list) (symbol : int * int) : int =
  let touched_numbers =
    List.filter (fun number -> is_number_touching_symbol number symbol) numbers
  in
  if List.length touched_numbers <> 2 then 0
  else
    let number1 = List.nth touched_numbers 0 in
    let number2 = List.nth touched_numbers 1 in
    number1.value * number2.value

let logic (lines : string list) : int =
  let symbols = get_all_symbol_positions lines r_symbols in
  let numbers = get_all_numbers_positions lines in
  let filter = is_numer_touching_any_symbol symbols in
  let numbers = List.filter filter numbers in
  List.fold_left (fun acc x -> acc + x.value) 0 numbers

let logic2 (lines : string list) : int =
  let symbols = get_all_symbol_positions lines r_star in
  let numbers = get_all_numbers_positions lines in
  let callback = get_gear_power numbers in
  let gear_powers = List.map callback symbols in
  List.fold_left (fun acc x -> acc + x) 0 gear_powers

let real_input = Parse.read "../inputs/day_3.txt"

let run () =
  let result = logic real_input in
  print_int result

(* Part 1 *)

(* I add % symbol to check the end of the numbers or correctly calulated *)
(* I add * symbol to check it is not take into account in the second part *)
(* let test_input = " *)
   (* 467..114.%  *)
   (* ...*...... *)
   (* ..35..633. *)
   (* ......#... *)
   (* 617*...... *)
   (* .....+.58. *)
   (* ..592..... *)
   (* ......755* *)
   (* ...$.*.... *)
   (* .664.598..  *)
   (* " *)
(**)
(* let%test_unit "get_symbol_position" =  *)
(*   let expected = [9; 8; 7; 4; 3; 1; 0] in *)
(*   [%test_eq: int list] (get_symbol_position "##.$#..*+#" r_symbols) expected *)
(**)
(* let%test_unit "get_numbers_positions" = *)
(*   let expected = [114; 467] in *)
(*   [%test_eq: int list] (List.map (fun x -> x.value) (get_numbers_positions "467..114.." 0)) expected *)
(**)
(* let%test_unit "get_numbers_positions" = *)
(*   let expected = [5; 0] in *)
(*   [%test_eq: int list] (List.map (fun x -> x.s) (get_numbers_positions "467..114.." 0)) expected *)
(**)
(* let%test_unit "get_numbers_positions" = *)
(*   let expected = [7; 2] in *)
(*   [%test_eq: int list] (List.map (fun x -> x.e) (get_numbers_positions "467..114.." 0)) expected *)
(**)
(* let%test_unit "logic" =   *)
(*   let expected = 4361 in *)
(*   [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic" =   *)
(*   let expected = 539590 in *)
(*   [%test_eq: int] (logic (real_input)) expected *)
(**)
(**)
(* (* Part 2 *) *)
(**)
(* let%test_unit "get_symbol_position" =  *)
(*   let expected = [7] in *)
(*   [%test_eq: int list] (get_symbol_position "##.$#..*+#" r_star) expected *)
(**)
(* let%test_unit "logic2" = *)
(*   let expected = 467835 in *)
(*   [%test_eq: int] (logic2 (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic2" = *)
(*   let expected = 80703636 in *)
(*   [%test_eq: int] (logic2 (real_input)) expected *)
