open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

(* data structures *)

type dir = U | D | L | R
type inst = { dir : dir; steps : int; color : string }

(* utilities *)

let count_chars_in_map (map : map) (chars : char list) : int =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in
  let prev_line_count = ref 0 in
  let line_count = ref 0 in

  let rec aux (x : int) (y : int) (count : int) : int =
    if y >= max_y then count
    else if x >= max_x then (
      if !line_count <> !prev_line_count then
        Printf.printf "y: %d, count: %d\n" (y - 1) !line_count;
      prev_line_count := !line_count;
      line_count := 0;
      aux 0 (y + 1) count)
    else
      let c = map.(y).(x) in
      let add = if mem c chars then 1 else 0 in
      line_count := !line_count + add;

      aux (x + 1) y (count + add)
  in
  aux 0 0 0

(* input *)

let test_input =
  Parse.get_lines
    "R 6 (#70c710)\n\
     D 5 (#0dc571)\n\
     L 2 (#5713f0)\n\
     D 2 (#d2c081)\n\
     R 2 (#59c680)\n\
     D 2 (#411b91)\n\
     L 5 (#8ceee2)\n\
     U 2 (#caa173)\n\
     L 1 (#1b58a2)\n\
     U 2 (#caa171)\n\
     R 2 (#7807d2)\n\
     U 3 (#a77fa3)\n\
     L 2 (#015232)\n\
     U 2 (#7a21e3)"

(* parsing *)

let get_dir (dir : string) : dir =
  let dir = dir.[0] in
  match dir with
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | 'R' -> R
  | _ -> failwith "invalid direction"

let parse_line (line : string) : inst =
  let parts = String.split_on_char ' ' line in
  let dir = parts |> hd |> get_dir in
  let steps = parts |> tl |> hd |> int_of_string in
  let color = parts |> tl |> tl |> hd in
  { dir; steps; color }

let get_instr (input : string t) = List.map parse_line input

(* logic *)

(* logic 1 *)

let move (path : path) (inst : inst) : path =
  let { dir; steps; _ } = inst in
  let last = List.hd path in
  let tail = List.tl path in
  let new_path = ref [ last ] in
  for _ = 1 to steps do
    let { x; y } = !new_path |> List.hd in
    let new_pos =
      match dir with
      | U -> { x; y = y - 1 }
      | D -> { x; y = y + 1 }
      | L -> { x = x - 1; y }
      | R -> { x = x + 1; y }
    in
    new_path := new_pos :: !new_path
  done;
  (* draw_path (List.rev !new_path); *)
  !new_path @ tail

let fill_map (map : map) : map =
  let map = add_outside_points map '.' in
  flood map 'o' '.'

let adjust_path (path : path) : path =
  let { x = min_x; y = min_y } = get_path_min path in
  let new_path = ref [] in

  let rec loop (path : path) =
    match path with
    | [] -> ()
    | { x; y } :: tail ->
      let new_pos = { x = x - min_x; y = y - min_y } in
      new_path := new_pos :: !new_path;
      loop tail
  in
  loop path;

  !new_path

let logic1 (lines : string list) : int =
  let instr = get_instr lines in
  let path = [ { x = 0; y = 0 } ] in
  let path = List.fold_left move path instr in
  let path = adjust_path path in

  (* draw_path path; *)
  let map = map_from_path path '|' '.' in
  let map = fill_map map in

  print_map map;
  count_chars_in_map map [ '.'; '|' ]

(* main *)

let run (path : string) =
  let input = Parse.read path in
  (* let input = test_input in *)
  let result = logic1 input in
  Printf.printf "%d\n" result

(* tests - Part 1 *)

let%test_unit "logic1" =
  let expected = 62 in
  [%test_eq: int] (logic1 test_input) expected

let%test_unit "logic1" =
  let real_input = Parse.read "../inputs/day_18.txt" in
  let expected = 49897 in
  [%test_eq: int] (logic1 real_input) expected

(* tests - Part 2 *)
