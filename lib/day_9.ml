open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* utilities *)

(* data structures *)

type line = int list

(* parsing *)

let get_line (line : string) : line =
  line |> String.split_on_char ' ' |> map int_of_string

(* logic *)

(* logic 1 *)

let get_next_line (line : line) : line =
  let rec aux (line : line) (acc : line) : line =
    match line with
    | [] -> rev acc
    | [ _ ] -> rev acc
    | x :: y :: xs -> aux (y :: xs) ((y - x) :: acc)
  in
  aux line []

let get_lines (line : line) : line list =
  let lines : line list = [ line ] in

  (* loop by adding the next line to the list until the next line is only composed of 0*)
  let rec aux (lines : line list) : line list =
    let next_line = get_next_line (hd lines) in
    let has_zero = next_line |> List.for_all (fun x -> x = 0) in
    if has_zero then next_line :: lines else aux (next_line :: lines)
  in

  aux lines

let get_lines_result (lines : line list) : int =
  let rec aux (lines : line list) (acc : int) : int =
    match lines with
    | [] -> acc
    | line :: xs ->
      let last = line |> rev |> List.hd in
      aux xs (acc + last)
  in
  aux lines 0

let get_line_result (line : line) : int =
  let lines = get_lines line in
  get_lines_result lines

(* lines |> map (fun x -> x |> fold_left ( + ) 0) |> fold_left ( + ) 0 0 *)

let logic (input : string list) : int =
  let lines = input |> map get_line in
  lines |> map get_line_result |> fold_left ( + ) 0

(* tests - Part 1 *)

(* logic 2 *)

(* main *)

let run (path : string) =
  let input = Parse.read path in
  let result = logic input in
  print_int result

(* tests - Part 1 *)

let test_input = "\n0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45\n"

let%test_unit "logic" =
  let expected = 114 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_9.txt" in
  let expected = 1762065988 in
  [%test_eq: int] (logic real_input) expected
