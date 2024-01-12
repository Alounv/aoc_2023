open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

let use_cache = true

(* data structures *)

type group = { size : int; start : int }
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

(* logic *)

let is_continuous (line : char array) (group : group) : bool =
  let sub = Array.sub line group.start group.size in
  sub |> Array.for_all (fun c -> c <> '.')

let get_is_path_valid (line : char array) (groups : group list) (index : int) :
  bool =
  if index < 0 then true
  else if index < Array.length line - 1 && line.(index + 1) = '#' then false
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

let get_is_path_really_valid (line : char array) (new_path : path)
    (prev_group_end : int) (len : int) (is_last : bool) : bool =
  let is_valid_so_far = get_is_path_valid line new_path prev_group_end in
  if is_valid_so_far = false then false
  else if is_last && not (get_is_path_valid line new_path (len - 1)) then false
  else true

let get_possible_starts (line : char array) (min_start : int) (max_end : int)
    (size : int) : int list =
  let rec aux (start : int) (acc : int list) : int list =
    if start + size > max_end then acc
    else
      let is_valid = is_continuous line { start; size } in
      let new_acc = if is_valid then start :: acc else acc in
      aux (start + 1) new_acc
  in
  aux min_start []

let print_path (path : path) =
  Printf.printf "path: %s\n %!"
    (path
     |> List.map (fun g -> Printf.sprintf "%d,%d" g.start g.size)
     |> String.concat " ")

let draw_path (path : path) (max : int) (is_valid : bool) =
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
  Printf.printf "%s: " (if is_valid then " " else "x");
  line |> Array.to_list |> List.iter (Printf.printf "%c");
  print_newline ()

let count_paths (line : char array) (groups_sizes : int list) : int =
  let cache : (string, int) Hashtbl.t = Hashtbl.create 100_000 in
  let len = Array.length line in

  let get (key : string) : int option =
    if Hashtbl.mem cache key then
      let result = Hashtbl.find cache key in
      (* Printf.printf "get: %s %d\n" key result; *)
      Some result
    else None
  in

  let store (key : string) (value : int) : unit =
    (* Printf.printf "store: %s - %d\n" key value; *)
    Hashtbl.add cache key value
  in

  let rec recursive (groups_sizes : int list) (prev_path : path) : int =
    let prev_group = List.hd prev_path in
    let prev_group_end = prev_group.size + prev_group.start - 1 in
    let is_valid =
      get_is_path_really_valid line prev_path prev_group_end len
        (groups_sizes = [])
    in
    if not is_valid then 0
    else
      let remaining_groups_count = List.length groups_sizes in
      let key = Printf.sprintf "%d-%d" prev_group_end remaining_groups_count in

      let cache_opt = get key in
      match cache_opt with
      | Some value -> value
      | None -> (
          let () = draw_path prev_path (prev_group_end + 1) is_valid in
          match groups_sizes with
          | [] -> 1
          | size :: tl ->
            let min_start =
              if prev_group.size = 0 then 0 else prev_group_end + 2
            in
            (* + 2 because we need at least one dot between groups *)
            let max_end =
              len - (tl |> List.fold_left (fun acc s -> acc + s + 1) 0)
            in
            let possible_starts =
              get_possible_starts line min_start max_end size
            in

            let paths =
              possible_starts
              |> List.map (fun start ->
                  let new_group = { size; start } in
                  let new_path : path = new_group :: prev_path in
                  let paths = recursive tl new_path in
                  paths)
            in
            let paths_count = List.fold_left ( + ) 0 paths in

            if use_cache then store key paths_count else ();

            (* Printf.printf "calc: %s - %d\n" retrieve_key paths_count; *)
            paths_count)
  in

  recursive groups_sizes [ { size = 0; start = 0 } ]

let parse_input (l : string) : char array * int list =
  let parts = String.split_on_char ' ' l in
  let line = List.hd parts in
  let line = line |> String.to_seq |> Array.of_seq in
  let groups = List.nth parts 1 in
  let groups_sizes =
    groups |> String.split_on_char ',' |> List.map int_of_string
  in
  (line, groups_sizes)

let parse_input2 (l : string) : char array * int list =
  let parts = String.split_on_char ' ' l in

  let left = List.hd parts in
  let left = left ^ "?" ^ left ^ "?" ^ left ^ "?" ^ left ^ "?" ^ left in
  let line = left |> String.to_seq |> Array.of_seq in

  let right = List.nth parts 1 in
  let right = right ^ "," ^ right ^ "," ^ right ^ "," ^ right ^ "," ^ right in

  Printf.printf " : %s %s\n" left right;
  let groups_sizes =
    right |> String.split_on_char ',' |> List.map int_of_string
  in
  (line, groups_sizes)

let logic (input : string list) =
  input
  |> List.fold_left
    (fun acc l ->
       let line, groups_sizes = parse_input l in
       let paths = count_paths line groups_sizes in
       Printf.printf "%s - %d\n" l paths;
       acc + paths)
    0

let logic2 (input : string list) =
  input
  |> List.fold_left
    (fun acc l ->
       let line, groups_sizes = parse_input2 l in
       let paths = count_paths line groups_sizes in
       Printf.printf "%s - %d\n %!" l paths;
       acc + paths)
    0

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let result = logic2 test_input in
  print_int result

(* tests - Part 1 *)

let%test_unit "get_group_possible_start" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let expected = [ 9; 8; 4; 0 ] in
  [%test_eq: int list] (get_possible_starts line 0 12 3) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???#" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 0 }; { size = 2; start = 4 } ] in
  let index = 12 in
  let expected = false in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???" |> String.to_seq |> Array.of_seq in
  let path = [ { size = 3; start = 4 }; { size = 5; start = 8 } ] in
  let index = 12 in
  let expected = true in
  [%test_eq: bool] (get_is_path_valid line path index) expected

let%test_unit "get_is_path_valid" =
  let line = "???.###.##???" |> String.to_seq |> Array.of_seq in
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
  let paths = count_paths chars groups_sizes in
  [%test_eq: int] paths 10

let%test_unit "parse_input" =
  let line = "?###????????" in
  let groups = "3,2,1" in
  let expected = (line |> String.to_seq |> Array.of_seq, [ 3; 2; 1 ]) in
  [%test_eq: char array * int list] (parse_input (line ^ " " ^ groups)) expected

let%test_unit "get_paths" =
  let input = "????????..?????#?#?? 3,5" in
  let chars, groups_sizes = parse_input input in
  let paths = count_paths chars groups_sizes in
  [%test_eq: int] paths 21

let%test_unit "get_paths" =
  let expected = 21 in
  [%test_eq: int] (logic test_input) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_12.txt" in
  let expected = 7674 in
  [%test_eq: int] (logic real_input) expected

(* tests - Part 2 *)

let%test_unit "get_paths" =
  let input = "?###???????? 3,2,1" in
  let chars, groups_sizes = parse_input2 input in
  let paths = count_paths chars groups_sizes in
  [%test_eq: int] paths 506250

let%test_unit "get_paths" =
  let expected = 525152 in
  [%test_eq: int] (logic2 test_input) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_12.txt" in
  let expected = 4443895258186 in
  [%test_eq: int] (logic2 real_input) expected
