open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
(* open Utilities *)

(* data structures *)

type group = { size : int; start : int }

(* corresponding to the starting position of each group *)
type path = group list

(* input *)

let test_input =
  Parse.get_lines
    "???.### 1,1,3\n\
     .??..??...?##. 1,1,3\n\
     ?#?#?#?#?#?#?#? 1,3,1,6\n\
     ????.#...#... 4,1,1\n\
     ????.######..#####. 1,6,5\n\
     ?###???????? 3,2,1"

let get_is_valid (line : char array) (start : int) (size : int) : bool =
  let sub = Array.sub line start size in
  sub |> Array.for_all (fun c -> c <> '.')

let get_is_path_valid (line : char array) (groups : group list) (index : int) :
  bool =
  if index < 0 then true
  else
    let len = index + 1 in
    let line = Array.sub line 0 len in
    let asserted_errors =
      line |> Array.to_list
      |> List.mapi (fun i c -> (i, c))
      |> List.filter (fun (_, c) -> c = '#')
    in

    asserted_errors
    |> List.for_all (fun (i, _) ->
        groups |> List.exists (fun g -> g.start <= i && i < g.start + g.size))

let get_group_possible_start (line : char array) (min_start : int)
    (max_end : int) (size : int) : int list =
  let rec aux (start : int) (acc : int list) : int list =
    if start + size > max_end then acc
    else
      let is_valid = get_is_valid line start size in
      let new_acc = if is_valid then start :: acc else acc in
      aux (start + 1) new_acc
  in
  aux min_start []

let print_path (path : path) =
  Printf.printf "path: %s\n"
    (path
     |> List.map (fun g -> Printf.sprintf "%d,%d" g.start g.size)
     |> String.concat " ")

let draw_path (path : path) (max : int) =
  let rec aux (path : path) (acc : char array) : char array =
    match path with
    | [] -> acc
    | { size; start } :: tl ->
      let new_acc =
        acc
        |> Array.mapi (fun i c ->
            if start <= i && i < start + size then '#' else c)
      in
      aux tl new_acc
  in
  let line = Array.make max '.' in
  let line = aux path line in
  line |> Array.to_list |> List.iter (Printf.printf "%c");
  print_newline ()

let get_paths (line : char array) (groups_sizes : int list) : path list =
  let len = Array.length line in

  let rec aux (groups_sizes : int list) (previous_group : group)
      (previous_path : path) : path list =
    let last_group_end = previous_group.size + previous_group.start - 1 in
    let is_path_valid = get_is_path_valid line previous_path last_group_end in
    if is_path_valid = false then []
    else
      match groups_sizes with
      | [] ->
        let is_valid = get_is_path_valid line previous_path (len - 1) in
        if is_valid then [ previous_path ] else []
      | size :: tl ->
        let min_start =
          if previous_group.size = 0 then 0 else last_group_end + 2
        in
        (* + 2 because we need at least one dot between groups *)
        let max_end = len - (tl |> List.fold_left ( + ) 0) in
        let possible_starts =
          get_group_possible_start line min_start max_end size
        in

        let paths =
          possible_starts
          |> List.map (fun start ->
              let new_group = { size; start } in
              let new_path : path = new_group :: previous_path in
              let paths = aux tl new_group new_path in
              paths)
        in
        paths |> List.concat
  in

  aux groups_sizes { size = 0; start = 0 } []

let parse_input (l : string) : char array * int list =
  let parts = String.split_on_char ' ' l in
  let line = List.hd parts in
  let line = line |> String.to_seq |> Array.of_seq in
  let groups = List.nth parts 1 in
  let groups_sizes =
    groups |> String.split_on_char ',' |> List.map int_of_string
  in
  (line, groups_sizes)

let logic (input : string list) =
  input
  |> List.fold_left
    (fun acc l ->
       let line, groups_sizes = parse_input l in
       let paths = get_paths line groups_sizes in
       let len = List.length paths in
       Printf.printf "%s - %d\n" l len;
       acc + len)
    0

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let result = logic test_input in
  print_int result

(* tests - Part 1 *)

let%test_unit "get_is_valid" =
  let line = "???.###.##??" |> String.to_seq |> Array.of_seq in
  let start = 7 in
  let size = 3 in
  let expected = false in
  [%test_eq: bool] (get_is_valid line start size) expected

let%test_unit "get_group_possible_start" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let expected = [ 9; 8; 4; 0 ] in
  [%test_eq: int list] (get_group_possible_start line 0 12 3) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 0 }; { size = 2; start = 4 } ] in
  let index = 12 in
  let expected = false in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 4 }; { size = 5; start = 8 } ] in
  let index = 12 in
  let expected = true in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 4 }; { size = 5; start = 8 } ] in
  let index = 12 in
  let expected = true in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let%test_unit "get_is_path_valid" =
  let line = "????????..?????#?#??" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 5 }; { size = 5; start = 10 } ] in
  let index = Array.length line - 1 in
  let expected = false in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let get_string_groups (paths : path list) : string =
  paths
  |> List.map (fun path ->
      path
      |> List.map (fun g -> Printf.sprintf "%d" g.start)
      |> String.concat " ")
  |> String.concat "\n"

let%test_unit "get_paths" =
  let input = "?###???????? 3,2,1" in
  let chars, groups_sizes = parse_input input in
  let paths = get_paths chars groups_sizes in
  [%test_eq: int] (List.length paths) 10

let%test_unit "parse_input" =
  let line = "?###????????" in
  let groups = "3,2,1" in
  let expected = (line |> String.to_seq |> Array.of_seq, [ 3; 2; 1 ]) in
  [%test_eq: char array * int list] (parse_input (line ^ " " ^ groups)) expected

let%test_unit "get_paths" =
  let input = "????????..?????#?#?? 3,5" in
  let chars, groups_sizes = parse_input input in
  let paths = get_paths chars groups_sizes in
  print_endline "";
  print_string input;
  print_endline "";
  let max = Array.length chars in
  paths |> List.iter (fun path -> draw_path path max);
  print_endline "";
  [%test_eq: int] (List.length paths) 21

let%test_unit "get_paths" =
  let expected = 21 in
  [%test_eq: int] (logic test_input) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_12.txt" in
  let expected = 7674 in
  [%test_eq: int] (logic real_input) expected
