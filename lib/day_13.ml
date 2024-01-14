open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

(* Lucas's logic :: Thanks !!! *)

(* data structures *)

(* utilities *)

(* input *)

let test_input =
  Parse.get_lines
    "#.##..##.\n\
     ..#.##.#.\n\
     ##......#\n\
     ##......#\n\
     ..#.##.#.\n\
     ..##..##.\n\
     #.#.##.#."

let test_input_2 =
  Parse.get_lines
    "#...##..#\n\
     #....#..#\n\
     ..##..###\n\
     #####.##.\n\
     #####.##.\n\
     ..##..###\n\
     #....#..#"

(* input parsing *)

let parse_tables (input : string) : map list =
  let tables = Str.split (Str.regexp "\n\n") input in
  tables |> List.map Parse.get_lines |> List.map map_from_strings

(* main logic *)

let is_sym_axis (map : map) (col : int) : bool =
  let n_rows, n_cols = get_map_dim map in

  let smallest_side = min (col + 1) (n_cols - col - 1) in

  let rec aux (j : int) (i : int) =
    if i >= n_rows then true
    else if j >= smallest_side then aux 0 (i + 1) (* go to next line *)
    else
      let left = map.(i).(col - j) in
      let right = map.(i).(col + j + 1) in
      if left = right then aux (j + 1) i else false
  in
  aux 0 0

let find_v_axis (map : map) : int =
  let _, n_cols = get_map_dim map in

  let rec aux (j : int) =
    if j > n_cols - 2 then -1 else if is_sym_axis map j then j else aux (j + 1)
  in
  aux 0

let find_h_axis (map : map) : int = map |> transpose_map |> find_v_axis

let logic (input : string) =
  input |> parse_tables
  |> List.fold_left
    (fun acc table ->
       let new_value =
         let v_axis = find_v_axis table in
         if v_axis >= 0 then v_axis + 1
         else
           let h_axis = find_h_axis table in
           (h_axis + 1) * 100
       in
       new_value + acc)
    0

(* Part 2 *)

let is_sym_axis_with_smudge (map : map) (col : int) : bool =
  let n_rows, n_cols = get_map_dim map in

  let smallest_side = min (col + 1) (n_cols - col - 1) in

  let rec aux (j : int) (i : int) (count : int) =
    if count > 1 then false
    else if i >= n_rows then count = 1
    else if j >= smallest_side then aux 0 (i + 1) count (* go to next line *)
    else
      let left = map.(i).(col - j) in
      let right = map.(i).(col + j + 1) in
      let new_smudges_count = if left = right then count else count + 1 in
      aux (j + 1) i new_smudges_count
  in
  aux 0 0 0

let find_v_axis_with_smudge (map : map) : int =
  let _, n_cols = get_map_dim map in

  let rec aux (j : int) =
    if j > n_cols - 2 then -1
    else if is_sym_axis_with_smudge map j then j
    else aux (j + 1)
  in
  aux 0

let find_h_axis_with_smudge (map : map) : int =
  map |> transpose_map |> find_v_axis_with_smudge

let logic2 (input : string) =
  input |> parse_tables
  |> List.fold_left
    (fun acc table ->
       let new_value =
         let v_axis = find_v_axis_with_smudge table in
         if v_axis >= 0 then v_axis + 1
         else
           let h_axis = find_h_axis_with_smudge table in
           (h_axis + 1) * 100
       in
       new_value + acc)
    0

(* main *)

let run (path : string) =
  let input = Parse.read path |> String.concat "\n" in
  let result = logic input in
  print_int result

(* tests - Part 1 *)

let%test_unit "is_sym_axis" =
  let map = map_from_strings test_input in
  let expected = true in
  [%test_eq: bool] (is_sym_axis map 4) expected

let%test_unit "is_sym_axis" =
  let map = map_from_strings test_input in
  let expected = false in
  [%test_eq: bool] (is_sym_axis map 3) expected

let%test_unit "is_sym_axis" =
  let map = map_from_strings test_input in
  let expected = false in
  [%test_eq: bool] (is_sym_axis map 7) expected

let%test_unit "is_sym_axis" =
  let map = map_from_strings test_input in
  let expected = false in
  [%test_eq: bool] (is_sym_axis map 0) expected

let%test_unit "find_vertical_axis" =
  let map = map_from_strings test_input in
  let expected = 4 in
  [%test_eq: int] (find_v_axis map) expected

let%test_unit "find_horizontal_axis" =
  let map = map_from_strings test_input_2 in
  print_map map;
  let expected = 3 in
  [%test_eq: int] (find_h_axis map) expected

let%test_unit "find_horizontal_axis" =
  let input =
    "...###.#.....##..\n\
     #.######.#.......\n\
     #.##.###.#.......\n\
     ...###.#.....##..\n\
     .#..##..#.###..##\n\
     #..###.####...###\n\
     #####.#...#.##...\n\
     #####.#..###.####\n\
     #########.#.#####\n\
     .#####....##..###\n\
     ##.#..#..####..##"
  in
  let map = map_from_strings (Parse.get_lines input) in
  print_map map;
  let expected = -1 in
  [%test_eq: int] (find_h_axis map) expected

let%test_unit "logic" =
  let input =
    "#.##..##.\n\
     ..#.##.#.\n\
     ##......#\n\
     ##......#\n\
     ..#.##.#.\n\
     ..##..##.\n\
     #.#.##.#.\n\n\
     #...##..#\n\
     #....#..#\n\
     ..##..###\n\
     #####.##.\n\
     #####.##.\n\
     ..##..###\n\
     #....#..#"
  in
  let expected = 405 in
  [%test_eq: int] (logic input) expected

let%test_unit "logic" =
  let input =
    "...###.#.....##..\n\
     #.######.#.......\n\
     #.##.###.#.......\n\
     ...###.#.....##..\n\
     .#..##..#.###..##\n\
     #..###.####...###\n\
     #####.#...#.##...\n\
     #####.#..###.####\n\
     #########.#.#####\n\
     .#####....##..###\n\
     ##.#..#..####..##"
  in
  let expected = 16 in
  [%test_eq: int] (logic input) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_13.txt" |> String.concat "\n" in
  let expected = 30535 in
  [%test_eq: int] (logic real_input) expected

(* tests - Part 2 *)

let%test_unit "logic" =
  let input =
    "#.##..##.\n\
     ..#.##.#.\n\
     ##......#\n\
     ##......#\n\
     ..#.##.#.\n\
     ..##..##.\n\
     #.#.##.#.\n\n\
     #...##..#\n\
     #....#..#\n\
     ..##..###\n\
     #####.##.\n\
     #####.##.\n\
     ..##..###\n\
     #....#..#"
  in
  let expected = 400 in
  [%test_eq: int] (logic2 input) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_13.txt" |> String.concat "\n" in
  let expected = 30844 in
  [%test_eq: int] (logic2 real_input) expected
