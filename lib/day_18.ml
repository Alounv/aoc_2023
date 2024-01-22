open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
(* open Utilities *)

(* data structures *)

type dir = U | D | L | R
type h_dir = L | R
type inst = { dir : dir; steps : int }
type point_int = { x : int; y : int; v : int }
type path_int = point_int list

let sort (path : path_int) : path_int =
  let compare a b = if a.y = b.y then a.x - b.x else a.y - b.y in
  List.sort compare path

(* utilities *)

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
  { dir; steps }

let get_instr (input : string t) = List.map parse_line input

(* logic *)

(* logic 1 *)

let move (path : path_int) (inst : inst) : path_int =
  let { dir; steps; _ } = inst in
  let { x; y; v } = List.hd path in

  let new_pos =
    match dir with
    | U -> { x; y = y - steps; v = v + 1 }
    | D -> { x; y = y + steps; v = v + 1 }
    | L -> { x = x - steps; y; v = v + 1 }
    | R -> { x = x + steps; y; v = v + 1 }
  in
  new_pos :: path

let get_path_min (path : path_int) : point_int =
  path
  |> List.fold_left
    (fun acc p -> { x = min acc.x p.x; y = min acc.y p.y; v = 0 })
    { x = 0; y = 0; v = 0 }

let adjust_path (path : path_int) : path_int =
  let { x = min_x; y = min_y; _ } = get_path_min path in

  let new_path = ref [] in
  let rec loop (path : path_int) =
    match path with
    | [] -> ()
    | { x; y; v } :: tail ->
      let new_pos = { x = x - min_x; y = y - min_y; v } in
      new_path := new_pos :: !new_path;
      loop tail
  in
  loop path;
  !new_path

let remove_last_if_duplicates_of_first (path : path_int) : path_int =
  let last = List.hd path in
  let first = List.hd (List.rev path) in
  if last.x = first.x && last.y = first.y then
    List.rev (List.tl (List.rev path))
  else path

type shape = L | N | U | M

let get_shape (current : int) (next : int) (line : int list) : shape =
  let s_up = List.mem current line in
  let e_up = List.mem next line in
  match (s_up, e_up) with
  | true, true -> U
  | true, false -> L
  | false, true -> N
  | false, false -> M

let get_line_value (line : int list) (new_points : int list) : int * int =
  let new_points = new_points |> List.rev in

  let first = List.hd new_points in
  let line_points_before =
    line |> List.filter (fun x -> x < first) |> List.length
  in
  let initial_was_in = line_points_before mod 2 = 1 in

  let rec loop (new_points : int list) (was_in : bool) (acc : int * int) :
    int * int =
    match new_points with
    | [] | [ _ ] -> acc
    | current :: next :: tail ->
      let shape = get_shape current next line in
      let is_closing =
        match shape with U | L -> not was_in | N | M -> was_in
      in
      let sub = match shape with U -> -1 | M -> 1 | L | N -> 0 in
      let cl = if is_closing then next - current - sub else 0 in
      let op = if is_closing then 0 else next - current + sub in

      (* this is wrong *)
      let has_next = match tail with [] -> false | _ -> true in
      let line_count_before_next =
        if has_next then
          line
          |> List.filter (fun x -> x > next && x < List.hd tail)
          |> List.length
        else 0
      in
      let was_in = match shape with U | M -> was_in | L | N -> not was_in in
      let was_in =
        if line_count_before_next mod 2 = 0 then was_in else not was_in
      in

      let o, c = acc in
      loop tail was_in (o + op, c + cl)
  in
  loop new_points initial_was_in (0, 0)

let cumulate_lines (line : int list) (new_points : int list) : int list =
  let rec loop (new_points : int list) (line : int list) : int list =
    match new_points with
    | [] -> line
    | current :: tail ->
      let line =
        if List.mem current line then List.filter (fun x -> x <> current) line
        else line @ [ current ]
      in
      loop tail line
  in
  loop new_points line

let count (path : path_int) : int =
  let max = List.length path - 1 in
  let first = List.hd path in

  let line : int list ref = ref [] in
  let area = ref 0 in
  let new_points = ref [ first.x ] in
  let prev_width = ref 0 in

  let rec loop (i : int) : unit =
    if i = max then area := !area + !prev_width
    else
      let left = List.nth path i in
      let right = List.nth path (i + 1) in

      if right.y <> left.y then (
        (* calculate area *)
        let opening, closing = get_line_value !line !new_points in
        line := cumulate_lines !line !new_points;
        let height = right.y - left.y in
        let width = !prev_width + opening - closing in
        if width < 0 then failwith "width < 0";
        area := !area + (width * height) + closing;

        (* reset *)
        new_points := [ right.x ];
        prev_width := width;

        (* Printf.printf "y: %d %d, count: %d, op: %d, cl: %d, add: %d\n" left.y *)
        (*   right.y width opening closing closing; *)

        (* continue *)
        loop (i + 1))
      else (
        new_points := right.x :: !new_points;
        loop (i + 1))
  in

  loop 0;
  !area

let logic1 (lines : string list) : int =
  let instr = get_instr lines in
  let path = [ { x = 0; y = 0; v = 0 } ] in
  let path = List.fold_left move path instr in
  let path = adjust_path path in
  let path = remove_last_if_duplicates_of_first path in
  let path = sort path in
  (* print_path (path |> List.map (fun x -> { x = x.x; y = x.y })); *)
  count path

(* logic 2 *)

let get_dir2 (dir : string) : dir =
  let dir = dir.[0] in
  match dir with
  | '0' -> R
  | '1' -> D
  | '2' -> L
  | '3' -> U
  | _ -> failwith "invalid direction"

let int_of_hex (hex : string) : int = int_of_string ("0x" ^ hex)

let parse_line2 (line : string) : inst =
  let parts = String.split_on_char '#' line in
  let parts = nth parts 1 in
  let steps = String.sub parts 0 5 |> int_of_hex in
  let dir = String.sub parts 5 1 |> get_dir2 in
  { dir; steps }

let get_instr2 (input : string t) = List.map parse_line2 input

let logic2 (lines : string list) : int =
  let instr = get_instr2 lines in
  let path = [ { x = 0; y = 0; v = 0 } ] in
  let path = List.fold_left move path instr in
  let path = adjust_path path in
  let path = remove_last_if_duplicates_of_first path in
  let path = sort path in
  (* print_path (path |> List.map (fun x -> { x = x.x; y = x.y })); *)
  count path

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

let%test_unit "logic2" =
  let expected = 952408144115 in
  [%test_eq: int] (logic2 test_input) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_18.txt" in
  let expected = 49897 in
  [%test_eq: int] (logic2 real_input) expected
