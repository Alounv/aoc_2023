(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open Utilities

(* data structures *)

type dir = N | E | S | W

(* utilities *)

(* input *)

let test_input =
  Parse.get_lines
    "O....#....\n\
     O.OO#....#\n\
     .....##...\n\
     OO.#O....O\n\
     .O.....O#.\n\
     O.#..O.#.#\n\
     ..O..#O..O\n\
     .......O..\n\
     #....###..\n\
     #OO..#...."

(* main logic *)

let tilt_N_step (map : map) : map =
  let { x = max_x; y = max_y } = get_map_max map in
  let new_map = Array.make_matrix (max_x + 1) (max_y + 1) '.' in

  for x = 0 to max_x do
    for y = 0 to max_y do
      let is_empty = map.(y).(x) = '.' in
      let is_next_mobile = y + 1 <= max_y && map.(y + 1).(x) = 'O' in
      let should_move = is_empty && is_next_mobile in
      if should_move then
        let () = new_map.(y).(x) <- 'O' in
        map.(y + 1).(x) <- '.'
      else new_map.(y).(x) <- map.(y).(x)
    done
  done;

  new_map

let tilt_N (map : map) : map =
  let rec loop (map : map) : map =
    let new_map = tilt_N_step map in
    if map = new_map then map else loop new_map
  in
  loop map

let count_load (map : map) : int =
  let { x = max_x; y = max_y } = get_map_max map in
  let count = ref 0 in
  for x = 0 to max_x do
    for y = 0 to max_y do
      if map.(y).(x) = 'O' then count := !count + max_y - y + 1
    done
  done;
  !count

let logic1 (input : string list) =
  let map = map_from_strings input in
  let stable_map = tilt_N map in
  let count = count_load stable_map in
  count

(* logic 2 *)

let cycle (map : map) : map =
  map |> tilt_N |> rotate_map_90 |> tilt_N |> rotate_map_90 |> tilt_N
  |> rotate_map_90 |> tilt_N |> rotate_map_90

let find_stable (map : map) : int * int =
  (* prevent mutation *)
  let map = clone_map map in

  let history = Hashtbl.create 100 in

  let rec loop (map : map) (count : int) : int * int =
    let new_map = cycle map in
    let key = string_from_map new_map in

    if Hashtbl.mem history key then (Hashtbl.find history key, count)
    else
      let () = Hashtbl.add history key count in
      loop new_map (count + 1)
  in
  loop map 0

let total = 1_000_000_000
(* let total = 20 + 141 *)

let logic2 (input : string list) =
  let map = map_from_strings input in
  let first, second = find_stable map in
  let equivalent = ((total - first) mod (second - first)) + first in
  Printf.printf "first: %d, second: %d, equivalent: %d\n" first second
    equivalent;
  let rec loop (map : map) (count : int) : map =
    if count = equivalent then map else loop (cycle map) (count + 1)
  in
  let final_map = loop map 0 in
  count_load final_map

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let load = logic2 test_input in
  Printf.printf "%d\n" load

(* (* tests - Part 1 *) *)
(**)
(* let%test_unit "tilt_N_step" = *)
(*   let expected = *)
(* "O.OO.#....\n\ *)
   (*      O...#....#\n\ *)
   (*      OO..O##..O\n\ *)
   (*      .O.#...O..\n\ *)
   (*      O....O..#.\n\ *)
   (*      ..#...O#.#\n\ *)
   (*      ..O..#.O.O\n\ *)
   (*      ..........\n\ *)
   (*      #OO..###..\n\ *)
   (*      #....#...." *)
(*   in *)
(*   let map = map_from_strings test_input in *)
(*   let result = tilt_N_step map in *)
(*   [%test_eq: string] (string_from_map result) expected *)
(**)
(* let%test_unit "tilt_north" = *)
(*   let expected = *)
(* "OOOO.#.O..\n\ *)
   (*      OO..#....#\n\ *)
   (*      OO..O##..O\n\ *)
   (*      O..#.OO...\n\ *)
   (*      ........#.\n\ *)
   (*      ..#....#.#\n\ *)
   (*      ..O..#.O.O\n\ *)
   (*      ..O.......\n\ *)
   (*      #....###..\n\ *)
   (*      #....#...." *)
(*   in *)
(*   let map = map_from_strings test_input in *)
(*   let result = tilt_N map in *)
(*   [%test_eq: string] (string_from_map result) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 136 in *)
(*   [%test_eq: int] (logic1 test_input) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_14.txt" in *)
(*   let expected = 109833 in *)
(*   [%test_eq: int] (logic1 real_input) expected *)
(**)
(* (* tests - Part 2 *) *)
(**)
(* let%test_unit "cycle 1" = *)
(*   let expected = *)
(* ".....#....\n\ *)
   (*      ....#...O#\n\ *)
   (*      ...OO##...\n\ *)
   (*      .OO#......\n\ *)
   (*      .....OOO#.\n\ *)
   (*      .O#...O#.#\n\ *)
   (*      ....O#....\n\ *)
   (*      ......OOOO\n\ *)
   (*      #...O###..\n\ *)
   (*      #..OO#...." *)
(*   in *)
(**)
(*   let map = map_from_strings test_input in *)
(*   let result = map |> cycle in *)
(*   [%test_eq: string] (string_from_map result) expected *)
(**)
(* let%test_unit "cycle 2" = *)
(*   let expected = *)
(* ".....#....\n\ *)
   (*      ....#...O#\n\ *)
   (*      .....##...\n\ *)
   (*      ..O#......\n\ *)
   (*      .....OOO#.\n\ *)
   (*      .O#...O#.#\n\ *)
   (*      ....O#...O\n\ *)
   (*      .......OOO\n\ *)
   (*      #..OO###..\n\ *)
   (*      #.OOO#...O" *)
(*   in *)
(**)
(*   let map = map_from_strings test_input in *)
(*   let result = map |> cycle |> cycle in *)
(*   [%test_eq: string] (string_from_map result) expected *)
(**)
(* let%test_unit "cycle 3" = *)
(*   let expected = *)
(* ".....#....\n\ *)
   (*      ....#...O#\n\ *)
   (*      .....##...\n\ *)
   (*      ..O#......\n\ *)
   (*      .....OOO#.\n\ *)
   (*      .O#...O#.#\n\ *)
   (*      ....O#...O\n\ *)
   (*      .......OOO\n\ *)
   (*      #...O###.O\n\ *)
   (*      #.OOO#...O" *)
(*   in *)
(**)
(*   let map = map_from_strings test_input in *)
(*   let result = map |> cycle |> cycle |> cycle in *)
(*   [%test_eq: string] (string_from_map result) expected *)
(**)
(* let%test_unit "cycle until stable" = *)
(*   let map = map_from_strings test_input in *)
(*   let first, second = find_stable map in *)
(*   [%test_eq: int list] [ first; second ] [ 2; 9 ] *)
(**)
(* let%test_unit "logic2" = *)
(*   let expected = 64 in *)
(*   [%test_eq: int] (logic2 test_input) expected *)
(**)
(* let%test_unit "logic2" = *)
(*   let real_input = Parse.read "../inputs/day_14.txt" in *)
(*   let expected = 99875 in *)
(*   [%test_eq: int] (logic2 real_input) expected *)
