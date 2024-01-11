open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

let flood_char = 'o'
let empty_char = ' '

(* input parsing *)

(* input *)
let test_input = Parse.get_lines "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ\n"

let test_input_2 =
  Parse.get_lines
    "FF7FSF7F7F7F7F7F---7\n\
     L|LJ||||||||||||F--J\n\
     FL-7LJLJ||||||LJL-77\n\
     F--JF--7||LJLJ7F7FJ-\n\
     L---JF-JLJ.||-FJLJJ7\n\
     |F|F-JF---7F7-L7L|7|\n\
     |FFJF7L7F-JF7|JL---7\n\
     7-L-JL7||F7|L7F-7F7|\n\
     L.L7LFJ|||||FJL7||LJ\n\
     L7JLJL-JLJLJL--JLJ.L\n"

(* data structures *)

type pipe = F | J | L | Seven | V | H | Nothing
type pipe_opt = pipe option

type neighbors = {
  top : pipe_opt;
  right : pipe_opt;
  bottom : pipe_opt;
  left : pipe_opt;
}

type direction = Top | Right | Bottom | Left

(* utilities *)

let coord_from_dir (point : point) (dir : direction) : point =
  match dir with
  | Top -> { x = point.x; y = point.y - 1 }
  | Right -> { x = point.x + 1; y = point.y }
  | Bottom -> { x = point.x; y = point.y + 1 }
  | Left -> { x = point.x - 1; y = point.y }

let get_start (lines : string list) : point =
  let rec aux (lines : string list) (y : int) : point =
    match lines with
    | [] -> { x = 0; y = 0 }
    | line :: lines ->
      let x = String.index_opt line 'S' in
      if x = None then aux lines (y + 1) else { x = Option.get x; y }
  in
  aux lines 0

let pipe_from_char (char : char) : pipe =
  match char with
  | 'F' -> F
  | 'J' -> J
  | 'L' -> L
  | '7' -> Seven
  | '|' -> V
  | '-' -> H
  | _ -> Nothing

let char_from_pipe (pipe : pipe) : char =
  match pipe with
  | F -> 'F'
  | J -> 'J'
  | L -> 'L'
  | Seven -> '7'
  | V -> '|'
  | H -> '-'
  | Nothing -> '.'

let pipe_from_coord (lines : string t) (point : point) : pipe_opt =
  if point.y < 0 || point.y >= List.length lines then None
  else
    let line = List.nth_opt lines point.y in
    if line = None then None
    else
      let line = Option.get line in
      if point.x < 0 || point.x >= String.length line then None
      else
        let char = String.get line point.x in
        Some (char |> pipe_from_char)

let get_close_points (lines : string t) (point : point) : neighbors =
  {
    top = pipe_from_coord lines { x = point.x; y = point.y - 1 };
    bottom = pipe_from_coord lines { x = point.x; y = point.y + 1 };
    left = pipe_from_coord lines { x = point.x - 1; y = point.y };
    right = pipe_from_coord lines { x = point.x + 1; y = point.y };
  }

let pipe_dir (pipe : pipe) : direction t =
  match pipe with
  | F -> [ Right; Bottom ]
  | J -> [ Left; Top ]
  | L -> [ Right; Top ]
  | Seven -> [ Left; Bottom ]
  | V -> [ Top; Bottom ]
  | H -> [ Left; Right ]
  | Nothing -> []

let get_opposite_dir (dir : direction) : direction =
  match dir with Top -> Bottom | Right -> Left | Bottom -> Top | Left -> Right

let is_valid (dir : direction) (next : pipe) : bool =
  next |> pipe_dir |> List.mem dir

let get_is_valid (current : pipe) (next : pipe) (dir : direction) : bool =
  let dir_from_current = pipe_dir current in
  if not (List.mem dir dir_from_current) then false
  else
    let next_dirs = pipe_dir next in
    let opposite_dir = get_opposite_dir dir in
    List.mem opposite_dir next_dirs

let replace_start (lines : string t) (start : point) : string t =
  let n = get_close_points lines start in
  let is_top =
    match n.top with None -> false | Some pipe -> is_valid Bottom pipe
  in
  let is_bottom =
    match n.bottom with None -> false | Some pipe -> is_valid Top pipe
  in
  let is_left =
    match n.left with None -> false | Some pipe -> is_valid Right pipe
  in
  let is_right =
    match n.right with None -> false | Some pipe -> is_valid Left pipe
  in

  let new_start =
    if is_right && is_bottom then F
    else if is_left && is_bottom then Seven
    else if is_right && is_top then L
    else if is_left && is_top then J
    else if is_top && is_bottom then V
    else if is_left && is_right then H
    else failwith "invalid start"
  in
  let new_start = char_from_pipe new_start in

  let lines =
    lines
    |> List.mapi (fun y line ->
        line
        |> String.mapi (fun x char ->
            if x = start.x && y = start.y then new_start else char))
  in
  lines

(* logic *)

let get_next_from_neigbors (current : pipe) (neighbors : neighbors)
    (from : direction) : pipe * direction =
  let possible_dirs = pipe_dir current |> List.filter (fun x -> x <> from) in
  let rec aux (dirs : direction list) : pipe * direction =
    match dirs with
    | [] -> failwith "no next"
    | dir :: dirs ->
      let next =
        match dir with
        | Top -> neighbors.top
        | Right -> neighbors.right
        | Bottom -> neighbors.bottom
        | Left -> neighbors.left
      in
      if next = None then aux dirs
      else
        let next = Option.get next in
        let is_valid = get_is_valid current next dir in
        if is_valid then (next, dir) else aux dirs
  in
  aux possible_dirs

let get_next (lines : string t) (point : point) (from : direction) :
  pipe * point * direction =
  let neighbors = get_close_points lines point in
  let current = pipe_from_coord lines point in
  if current = None then failwith "no current"
  else
    let current = Option.get current in
    let next, dir = get_next_from_neigbors current neighbors from in
    let next_coord = coord_from_dir point dir in
    (next, next_coord, dir)

let get_path (lines : string t) (start : point) (start_dir : direction) : path =
  let rec aux (lines : string t) (points : point t) (from : direction) : path =
    let last = points |> List.hd in
    let _, next_coord, next_dir = get_next lines last from in

    let is_back_to_start = next_coord = start in
    if is_back_to_start then points
    else
      let from = get_opposite_dir next_dir in
      aux lines (next_coord :: points) from
  in
  aux lines [ start ] start_dir

(* logic 1 *)

let logic (lines : string t) : int =
  let start = get_start lines in
  let lines = replace_start lines start in

  let path = get_path lines start Bottom in
  let path_length = List.length path in
  path_length / 2

(* logic 2 *)
type status = In | Out | Wall

let count_in_line (line : line) : int =
  let full_list = line |> Array.to_seq |> List.of_seq in

  (* let length = List.length full_list in *)
  let rec aux (list : char t) (count : int) (status : int) (last_turn : char) :
    int =
    match list with
    | [] -> count
    | char :: rest -> (
        match char with
        | '|' -> aux rest count (status + 1) last_turn
        | '-' -> aux rest count status last_turn
        | ' ' ->
          let count = if status mod 2 = 1 then count + 1 else count in
          aux rest count status last_turn
        | 'F' | 'L' ->
          let status = status in
          aux rest count status char
        | 'J' ->
          let status = if last_turn = 'F' then status + 1 else status in
          aux rest count status char
        | '7' ->
          let status = if last_turn = 'L' then status + 1 else status in
          aux rest count status char
        | _ -> aux rest count status last_turn)
  in

  let count = aux full_list 0 0 ' ' in
  count

let count_map (map : map) : int =
  let rec aux (map : line t) (count : int) : int =
    match map with
    | [] -> count
    | line :: rest ->
      let count = count + count_in_line line in
      aux rest count
  in

  let map = map |> Array.to_seq |> List.of_seq in
  aux map 0

let logic2 (lines : string t) : int =
  let start = get_start lines in
  let lines = replace_start lines start in

  let map = map_from_strings lines in
  let path = get_path lines start Left in

  let map = map_from_path_and_map path map empty_char in
  let map = add_outside_points map empty_char in
  let map = flood map flood_char empty_char in
  (* let map = add_outside_points map empty_char in *)
  print_map map;

  (* let map = flood map flood_char empty_char in *)
  (* print_map map; *)
  count_map map

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let result = logic test_input in
  print_int result

(* tests - Part 1 *)

(* let%test_unit "start" = *)
(*   let result = get_start test_input in *)
(*   let string_result = Printf.sprintf "%d %d" result.x result.y in *)
(*   [%test_eq: string] string_result "0 2" *)
(**)
(* let%test_unit "get_next" = *)
(*   let input = replace_start test_input { x = 0; y = 2 } in *)
(*   let result = get_next input { x = 0; y = 2 } Bottom in *)
(*   let pipe, point, _ = result in *)
(*   let pipe = char_from_pipe pipe in *)
(*   let string_result = Printf.sprintf "%c %d %d" pipe point.x point.y in *)
(*   [%test_eq: string] string_result "J 1 2" *)
(**)
(* let%test_unit "get_next" = *)
(*   let input = replace_start test_input { x = 0; y = 2 } in *)
(*   let result = get_next input { x = 1; y = 2 } Left in *)
(*   let pipe, point, _ = result in *)
(*   let pipe = char_from_pipe pipe in *)
(*   let string_result = Printf.sprintf "%c %d %d" pipe point.x point.y in *)
(*   [%test_eq: string] string_result "F 1 1" *)
(**)
(* let%test_unit "get_path" = *)
(*   let result = get_path test_input { x = 0; y = 2 } in *)
(*   let string_result = *)
(*     result *)
(*     |> List.map (fun x -> Printf.sprintf "(%d, %d)" x.x x.y) *)
(*     |> String.concat " " *)
(*   in *)
(*   [%test_eq: string] string_result *)
(* "(0, 3) (0, 4) (1, 4) (1, 3) (2, 3) (3, 3) (4, 3) (4, 2) (3, 2) (3, 1) (3, \ *)
   (*      0) (2, 0) (2, 1) (1, 1) (1, 2) (0, 2)" *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 8 in *)
(*   [%test_eq: int] (logic test_input) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_10.txt" in *)
(*   let expected = 7097 in *)
(*   [%test_eq: int] (logic real_input) expected *)

(* tests - Part 2 *)

let%test_unit "count_in_line" =
  let line = "| |   | |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| FJ   | |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| L7   | |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| L7   L7 |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| L7   FJ |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line =
    "| L------7   L--7 |" |> String.to_seq |> List.of_seq |> Array.of_list
  in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line =
    "| L------7L-7L-7   L--7 |" |> String.to_seq |> List.of_seq |> Array.of_list
  in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| L-----J |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "count_in_line" =
  let line = "| F-----7 |" |> String.to_seq |> List.of_seq |> Array.of_list in
  let result = count_in_line line in
  [%test_eq: int] result 2

let%test_unit "logic2" =
  let expected = 10 in
  [%test_eq: int] (logic2 test_input_2) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_10.txt" in
  let expected = 355 in
  [%test_eq: int] (logic2 real_input) expected
