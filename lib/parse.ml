open Ppx_compare_lib.Builtin
open Sexplib.Std

(* read the file and return the contents as a list of string *)
let read path = 
  let in_channel = open_in path in
  let lines = ref [] in
  try
    while true do
      let line = input_line in_channel in
      lines := line :: !lines
    done; !lines
  with End_of_file ->
    close_in in_channel;
    List.rev !lines ;;
;;

let remove_first_empty_line lines = 
  match lines with
  | [] -> []
  | first :: rest -> 
    if first = "" then rest
    else lines

let get_lines string_input = 
  let lines = String.split_on_char '\n' string_input in
  (* remove first and last empty lines *)
  let lines = remove_first_empty_line lines in
  let lines = remove_first_empty_line (List.rev lines) in
  List.rev lines
;;


(* test *)

let test_path = "../inputs/parse.txt"
let expected = ["first line"; "second line"; "third line"]
let%test_unit "read" = [%test_eq: string list] (read test_path) expected


let test_input = "
ligne 1
ligne 2, avec une virgule
ligne 3. avec un point
"
let expected_lines = ["ligne 1"; "ligne 2, avec une virgule"; "ligne 3. avec un point"]
let%test_unit "get_lines" = [%test_eq: string list] (get_lines test_input) expected_lines

