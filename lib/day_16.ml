open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

(* data structures *)

type dir = N | E | S | W
type beam = { x : int; y : int; dir : dir }

(* utilities *)

let print_dir (dir : dir) : char =
  match dir with N -> '^' | E -> '>' | S -> 'v' | W -> '<'

(* parsing *)

(* input *)

let test_input =
  Parse.get_lines
    ".|...\\....\n\
     |.-.\\.....\n\
     .....|-...\n\
     ........|.\n\
     ..........\n\
     .........\\\n\
     ..../.\\\\..\n\
     .-.-/..|..\n\
     .|....-|.\\\n\
     ..//.|...."

(* logic 1 *)

let get_next_position ({ x; y; dir } : beam) : point =
  match dir with
  | N -> { x; y = y - 1 }
  | E -> { x = x + 1; y }
  | S -> { x; y = y + 1 }
  | W -> { x = x - 1; y }

let get_next_beams (char : char) (dir : dir) ({ x; y } : point) : beam list =
  match char with
  | '.' -> [ { x; y; dir } ]
  | '|' -> (
      match dir with
      | N | S -> [ { x; y; dir } ]
      | E | W -> [ { x; y; dir = N }; { x; y; dir = S } ])
  | '-' -> (
      match dir with
      | N | S -> [ { x; y; dir = E }; { x; y; dir = W } ]
      | E | W -> [ { x; y; dir } ])
  | '/' -> (
      match dir with
      | N -> [ { x; y; dir = E } ]
      | E -> [ { x; y; dir = N } ]
      | S -> [ { x; y; dir = W } ]
      | W -> [ { x; y; dir = S } ])
  | '\\' -> (
      match dir with
      | N -> [ { x; y; dir = W } ]
      | E -> [ { x; y; dir = S } ]
      | S -> [ { x; y; dir = E } ]
      | W -> [ { x; y; dir = N } ])
  | _ -> []

let move_one (map : map) (beam : beam) : beam list =
  let new_p = get_next_position beam in
  if is_out_of_bounds map new_p then []
  else
    let char = map.(new_p.y).(new_p.x) in
    get_next_beams char beam.dir new_p

let move (map : map) (beams : beam list) : beam list =
  concat_map (move_one map) beams

let key_of_beam ({ x; y; _ } : beam) : string = Printf.sprintf "%d,%d" x y

let print_map =
  print_map_with_colors
    [
      { color = 7; chars = [ '|'; '-'; '/'; '\\' ] };
      { color = 5; chars = [ '^'; '>'; 'v'; '<'; '1'; '2'; '3'; '4' ] };
      { color = 10; chars = [ '.' ] };
    ]

let remove_duplicates (existing_beams : (string, dir t) Hashtbl.t)
    (beams : beam list) : beam list =
  beams
  |> List.filter (fun beam ->
      let key = key_of_beam beam in
      if Hashtbl.mem existing_beams key then
        let dirs = Hashtbl.find existing_beams key in
        let is_duplicate = dirs |> List.mem beam.dir in
        if is_duplicate then false
        else (
          Hashtbl.replace existing_beams key (beam.dir :: dirs);
          true)
      else (
        Hashtbl.add existing_beams key [ beam.dir ];
        true))

let update_beam_map (beam_map : map) (beams : beam list) : unit =
  beams
  |> List.iter (fun { x; y; dir } ->
      beam_map.(y).(x) <-
        (if [ '|'; '-'; '/'; '\\' ] |> List.mem beam_map.(y).(x) then
           beam_map.(y).(x)
         else if [ '^'; '>'; 'v'; '<' ] |> List.mem beam_map.(y).(x) then '2'
         else
           let code = int_of_char beam_map.(y).(x) in
           let is_int = code > 47 && code < 58 in
           if is_int then char_of_int (code + 1) else print_dir dir));
  ()

let count (map : map) (start : beam) : int =
  let existing_beams : (string, dir t) Hashtbl.t = Hashtbl.create 1000 in
  Hashtbl.add existing_beams (key_of_beam start) [ start.dir ];

  let beam_map = clone_map map in
  beam_map.(start.y).(start.x) <- print_dir start.dir;

  let rec aux (beams : beam list) : beam list =
    if length beams = 0 then []
    else
      let new_beams = move map beams in
      let new_beams = remove_duplicates existing_beams new_beams in
      update_beam_map beam_map new_beams;
      aux new_beams
  in

  let _ = aux [ start ] in
  print_map beam_map;
  existing_beams |> Hashtbl.length

let logic1 (lines : string list) (start : beam) : int =
  let map = map_from_strings lines in
  count map start

(* logic 2 *)

let get_possible_starts (map : map) : beam list =
  let max_x = Array.length map.(0) - 1 in
  let max_y = Array.length map - 1 in

  let list = ref [] in

  for x = 0 to max_x do
    list := { x; y = 0; dir = S } :: !list;
    list := { x; y = max_y; dir = N } :: !list
  done;

  for y = 0 to max_y do
    list := { x = 0; y; dir = E } :: !list;
    list := { x = max_x; y; dir = W } :: !list
  done;

  !list

let logic2 (lines : string list) : int =
  let map = map_from_strings lines in
  let possible_starts = get_possible_starts map in
  let counts = possible_starts |> List.map (count map) in
  counts |> List.fold_left max 0

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let load = logic1 test_input { x = 0; y = 0; dir = E } in
  Printf.printf "%d\n" load

(* tests - Part 1 *)

let%test_unit "logic1" =
  let expected = 46 in
  [%test_eq: int] (logic1 test_input { x = 0; y = 0; dir = E }) expected

let%test_unit "logic1" =
  let real_input = Parse.read "../inputs/day_16.txt" in
  let expected = 7034 in
  [%test_eq: int] (logic1 real_input { x = 0; y = 0; dir = S }) expected

(* tests - Part 2 *)

let%test_unit "logic2" =
  let expected = 51 in
  [%test_eq: int] (logic2 test_input) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_16.txt" in
  let expected = 7759 in
  [%test_eq: int] (logic2 real_input) expected
