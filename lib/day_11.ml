(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List
open Utilities

let empty = '.'

type star = { name : int; coord : point }
type pair = { a : star; b : star }

(* input *)

let test_input =
  Parse.get_lines
    "...#......\n\
     .......#..\n\
     #.........\n\
     ..........\n\
     ......#...\n\
     .#........\n\
     .........#\n\
     ..........\n\
     .......#..\n\
     #...#.....\n"

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

let get_stars (map : map) : star list =
  let coords = get_coords map '#' in
  coords |> List.mapi (fun i coord -> { name = i; coord })

let get_distance (a : point) (b : point) : int =
  let x = a.x - b.x |> abs in
  let y = a.y - b.y |> abs in
  x + y

let get_pairs (stars : star list) : pair list =
  let rec aux (a : star) (stars : star list) (acc : pair list) =
    match stars with
    | [] -> acc
    | b :: rest ->
      let pair = { a; b } in
      aux a rest (pair :: acc)
  in
  let rec aux2 (stars : star list) (acc : pair list) =
    match stars with
    | [] -> acc
    | a :: rest ->
      let pairs = aux a rest [] in
      aux2 rest (pairs @ acc)
  in
  aux2 stars []

(* logic 2 *)

let count_expanses_between (expanded_rows : int list)
    (expanded_columns : int list) (a : point) (b : point) : int =
  let rows =
    List.filter
      (fun y -> (y > a.y && y < b.y) || (y > b.y && y < a.y))
      expanded_rows
    |> List.length
  in
  let columns =
    List.filter
      (fun x -> (x > a.x && x < b.x) || (x > b.x && x < a.x))
      expanded_columns
    |> List.length
  in
  rows + columns

let get_distance2 (factor : int) (expanded_rows : int list)
    (expanded_columns : int list) (a : point) (b : point) : int =
  let distance = get_distance a b in

  let count = count_expanses_between expanded_rows expanded_columns in
  let expanses = count a b in
  let new_distance = distance + (expanses * (factor - 1)) in
  new_distance

let logic (input : string list) (factor : int) =
  let map = map_from_strings input in
  let empty_rows = get_empty_rows map in
  let empty_columns = get_empty_columns map in
  let stars = get_stars map in
  let pairs = get_pairs stars in
  let get_d = get_distance2 factor empty_rows empty_columns in
  let distances = List.map (fun { a; b } -> get_d a.coord b.coord) pairs in

  let sum = List.fold_left ( + ) 0 distances in
  sum

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let result = logic test_input 2 in
  print_int result

(* (* tests - Part 1 *) *)
(**)
(* let%test_unit "get_empty_columns" = *)
(*   let map = map_from_strings test_input in *)
(*   [%test_eq: int list] (get_empty_columns map) [ 8; 5; 2 ] *)
(**)
(* let%test_unit "get_empty_rows" = *)
(*   let map = map_from_strings test_input in *)
(*   [%test_eq: int list] (get_empty_rows map) [ 7; 3 ] *)
(**)
(* let%test_unit "get_distance" = *)
(*   let a = { x = 0; y = 0 } in *)
(*   let b = { x = 4; y = 5 } in *)
(*   let expected = 9 in *)
(*   [%test_eq: int] (get_distance a b) expected *)
(**)
(* let%test_unit "get_distance" = *)
(*   let a = { x = 0; y = 0 } in *)
(*   let b = { x = 4; y = 5 } in *)
(*   let expected = 9 in *)
(*   [%test_eq: int] (get_distance b a) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 374 in *)
(*   [%test_eq: int] (logic test_input 2) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_11.txt" in *)
(*   let expected = 9605127 in *)
(*   [%test_eq: int] (logic real_input 2) expected *)
(**)
(* (* tests - Part 2 *) *)
(**)
(* let%test_unit "get_stars" = *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_stars map in *)
(*   let result_string = *)
(*     result *)
(*     |> List.map (fun { name; coord } -> *)
(*         Printf.sprintf "%d: %d, %d\n" name coord.x coord.y) *)
(*     |> String.concat " " *)
(*   in *)
(*   let expected = *)
(* "0: 4, 9\n\ *)
   (*     \ 1: 0, 9\n\ *)
   (*     \ 2: 7, 8\n\ *)
   (*     \ 3: 9, 6\n\ *)
   (*     \ 4: 1, 5\n\ *)
   (*     \ 5: 6, 4\n\ *)
   (*     \ 6: 0, 2\n\ *)
   (*     \ 7: 7, 1\n\ *)
   (*     \ 8: 3, 0\n" *)
(*   in *)
(*   [%test_eq: string] result_string expected *)
(**)
(* let%test_unit "count_expanses_between_points" = *)
(*   let a = { x = 1; y = 1 } in *)
(*   let b = { x = 6; y = 4 } in *)
(*   let expanded_columns = [ 8; 5; 2 ] in *)
(*   let expanded_rows = [ 7; 3 ] in *)
(*   let expected = 3 in *)
(*   [%test_eq: int] *)
(*     (count_expanses_between expanded_rows expanded_columns a b) *)
(*     expected *)
(**)
(* let%test_unit "count_expanses_between_points" = *)
(*   let a = { x = 1; y = 1 } in *)
(*   let b = { x = 6; y = 4 } in *)
(*   let expanded_columns = [ 8; 5; 2 ] in *)
(*   let expanded_rows = [ 7; 3 ] in *)
(*   let expected = 3 in *)
(*   [%test_eq: int] *)
(*     (count_expanses_between expanded_rows expanded_columns b a) *)
(*     expected *)
(**)
(* let%test_unit "get_distance2" = *)
(*   let a = { x = 1; y = 1 } in *)
(*   let b = { x = 6; y = 4 } in *)
(*   let expanded_columns = [ 8; 5; 2 ] in *)
(*   let expanded_rows = [ 7; 3 ] in *)
(*   let expected = 8 in *)
(*   [%test_eq: int] (get_distance2 1 expanded_rows expanded_columns a b) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 374 in *)
(*   [%test_eq: int] (logic test_input 2) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 1030 in *)
(*   [%test_eq: int] (logic test_input 10) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 8410 in *)
(*   [%test_eq: int] (logic test_input 100) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_11.txt" in *)
(*   let expected = 458191688761 in *)
(*   [%test_eq: int] (logic real_input 1000000) expected *)
