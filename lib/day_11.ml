open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

let empty = '.'

type star = { name : int; coord : point }

(* input *)

let test_input =
  Parse.get_lines
    "\n\
     ...#......\n\
     .......#..\n\
     #.........\n\
     ..........\n\
     ......#...\n\
     .#........\n\
     .........#\n\
     ..........\n\
     .......#..\n\
     #...#.....\n"

(* utilities *)

(* logic *)

let get_is_list_empty (list : 'a list) : bool =
  let list = filter (fun x -> x <> empty) list in
  match list with [] -> true | _ -> false

let get_empty_columns (map : map) : int list =
  let max_x = Array.length map.(0) - 1 in
  let rec aux x acc =
    if x > max_x then acc
    else
      let column = get_column map x in
      let is_column_empty = get_is_list_empty column in
      if is_column_empty then aux (x + 1) (x :: acc) else aux (x + 1) acc
  in
  aux 0 []

let get_empty_rows (map : map) : int list =
  let max_y = Array.length map - 1 in
  let rec aux y acc =
    if y > max_y then acc
    else
      let row = map.(y) in
      let row = Array.to_list row in
      let is_row_empty = get_is_list_empty row in
      if is_row_empty then aux (y + 1) (y :: acc) else aux (y + 1) acc
  in
  aux 0 []

let expand_line (line : line) (to_double : int list) : line =
  let max_x = Array.length line - 1 in
  let rec aux (x : int) (acc : char list) =
    if x > max_x then acc
    else
      let is_to_double = mem x to_double in
      if is_to_double then aux (x + 1) (line.(x) :: line.(x) :: acc)
      else aux (x + 1) (line.(x) :: acc)
  in
  let list = aux 0 [] in
  list |> List.rev |> Array.of_list

let expand_map (map : map) : map =
  let empty_rows = get_empty_rows map in
  let empty_columns = get_empty_columns map in
  let max_y = Array.length map - 1 in

  let rec aux (y : int) (acc : line list) =
    if y > max_y then acc
    else
      let line = map.(y) in
      let line = expand_line line empty_columns in
      let is_to_double = mem y empty_rows in
      if is_to_double then aux (y + 1) (line :: line :: acc)
      else aux (y + 1) (line :: acc)
  in

  let new_list = aux 0 [] in
  new_list |> List.rev |> Array.of_list

let get_stars (map : map) : star list =
  let coords = get_coords map '#' in
  coords |> List.mapi (fun i coord -> { name = i; coord })

(* logic 1 *)

let logic (input : string list) =
  let _map = map_from_strings input in
  0

(* logic 2 *)

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let result = logic test_input in
  print_int result

(* tests - Part 1 *)

let%test_unit "get_empty_columns" =
  let map = map_from_strings test_input in
  [%test_eq: int list] (get_empty_columns map) [ 8; 5; 2 ]

let%test_unit "get_empty_rows" =
  let map = map_from_strings test_input in
  [%test_eq: int list] (get_empty_rows map) [ 7; 3 ]

let%test_unit "expand_line" =
  let line = [| '.'; '.'; 'A'; '#'; '.'; 'B'; '.'; '.'; 'C'; '.' |] in
  let to_double = [ 8; 5; 2 ] in
  let expected =
    [| '.'; '.'; 'A'; 'A'; '#'; '.'; 'B'; 'B'; '.'; '.'; 'C'; 'C'; '.' |]
  in
  let result = expand_line line to_double in
  let result_list = Array.to_list result in
  let expected_list = Array.to_list expected in
  [%test_eq: char list] result_list expected_list

let%test_unit "expand_map" =
  let map = map_from_strings test_input in
  let result = expand_map map in
  let string_result = string_from_map result in
  let expected =
    "....#........\n\
     .........#...\n\
     #............\n\
     .............\n\
     .............\n\
     ........#....\n\
     .#...........\n\
     ............#\n\
     .............\n\
     .............\n\
     .........#...\n\
     #....#......."
  in
  [%test_eq: string] string_result expected

let%test_unit "get_stars" =
  let map = map_from_strings test_input in
  let result = get_stars map in
  let result_string =
    result
    |> List.map (fun { name; coord } ->
        Printf.sprintf "%d: %d, %d\n" name coord.x coord.y)
    |> String.concat " "
  in
  let expected =
    "0: 4, 9\n\
    \ 1: 0, 9\n\
    \ 2: 7, 8\n\
    \ 3: 9, 6\n\
    \ 4: 1, 5\n\
    \ 5: 6, 4\n\
    \ 6: 0, 2\n\
    \ 7: 7, 1\n\
    \ 8: 3, 0\n"
  in
  [%test_eq: string] result_string expected

(* let%test_unit "logic" = *)
(*   let expected = 8 in *)
(*   [%test_eq: int] (logic test_input) expected *)

(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_11.txt" in *)
(*   let expected = 7097 in *)
(*   [%test_eq: int] (logic real_input) expected *)

(* tests - Part 2 *)
