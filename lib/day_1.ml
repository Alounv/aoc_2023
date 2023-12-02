open Ppx_compare_lib.Builtin
open Sexplib.Std
open List


let input = Parse.read "../inputs/day_1_1.txt"
let parse = Parse.get_lines

let is_digit c = 
  let code = Char.code c in
  code >= 48 && code <= 57

let get_line_first_digit line = 
  let rec loop i = 
    if i >= String.length line then None
    else 
      let c = line.[i] in
      if is_digit c then Some (i, int_of_char c - 48)
      else loop (i + 1) in
  loop 0
;;

let get_line_last_digit line = 
  let rec loop i = 
    if i < 0 then None
    else 
      let c = line.[i] in
      if is_digit c then Some (i, int_of_char c - 48)
      else loop (i - 1) in
  loop (String.length line - 1)
;;

let get_line_result line = 
  let first_digit = get_line_first_digit line in 
  let last_digit =  get_line_last_digit line in
  match first_digit, last_digit with
  | Some (i, d1), Some (j, d2) -> 
    if i = j then Some (d1)
    else if i != j then Some (d1 + d2)
    else None
  | _ -> None
;;
  

let logic input = 
let lines = parse input in 
let numbers = List.filter_map get_line_result lines in
List.fold_left (+) 0 numbers
;;



let run = logic

(* tests *)

let test_input = "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"
let expected = 20
let%test_unit "logic" = [%test_eq: int] (logic test_input) expected


