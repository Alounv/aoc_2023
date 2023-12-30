(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List

(* data structures *)

type transform = { s : int; e : int; inc : int }
type stage = { id : string; transforms : transform list }

(* parsing *)

let get_seeds (line : string) : int list =
  let parts = String.split_on_char ':' line in
  let seeds =
    List.nth parts 1 |> String.trim |> String.split_on_char ' '
    |> List.map int_of_string
  in
  seeds

let get_seeds_ranges (line : string) : (int * int) list =
  let parts = get_seeds line in
  (* group by 2 *)
  let rec loop (parts : int list) (ranges : (int * int) list) : (int * int) list
      =
    match parts with
    | [] -> ranges
    | start :: range :: rest ->
        let new_ranges = (start, start + range) :: ranges in
        loop rest new_ranges
    | _ -> failwith "invalid input"
  in
  loop parts []

let get_transform (line : string) : transform =
  let parts = String.split_on_char ' ' line in
  let values = List.map int_of_string parts in
  let destination = List.nth values 0 in
  let s = List.nth values 1 in
  let range = List.nth values 2 in
  { s; e = s + range; inc = destination - s }

let get_reverse_tranform (line : string) : transform =
  let parts = String.split_on_char ' ' line in
  let values = List.map int_of_string parts in
  let s = List.nth values 0 in
  let destination = List.nth values 1 in
  let range = List.nth values 2 in
  { s; e = s + range; inc = destination - s }

let get_stage (lines : string list) : stage =
  let id = List.nth lines 0 in
  let transforms = List.tl lines |> List.map get_transform in
  { id; transforms }

let get_reverse_stage (lines : string list) : stage =
  let id = List.nth lines 0 in
  let transforms = List.tl lines |> List.map get_reverse_tranform in
  { id; transforms }

let split_by_empty_line (lines : string list) : string list list =
  let rec loop (lines : string list) (stages : string list list) :
      string list list =
    match lines with
    | [] -> stages
    | "" :: rest -> loop rest ([] :: stages)
    | line :: rest_lines ->
        let c_stage = List.hd stages in
        let rest_stages = List.tl stages in
        let updated_stage = line :: c_stage in
        loop rest_lines (updated_stage :: rest_stages)
  in
  loop lines [ [] ] |> List.map List.rev |> List.rev

(* logic *)

let transform (transforms : transform list) (origin : int) : int =
  let rec loop (transforms : transform list) (origin : int) : int =
    match transforms with
    | [] -> origin
    | { s; e; inc } :: rest ->
        if origin >= s && origin <= e then origin + inc else loop rest origin
  in
  loop transforms origin

let combine_stages (stages : stage list) (origin : int) : int =
  let rec loop (stages : stage list) (origin : int) : int =
    match stages with
    | [] -> origin
    | stage :: rest ->
        let origin = transform stage.transforms origin in
        loop rest origin
  in
  loop stages origin

(* main *)

let logic (lines : string list) : int =
  (* parsing *)
  let seeds = get_seeds (List.hd lines) in
  let stages =
    List.tl lines |> List.tl |> split_by_empty_line |> List.map get_stage
  in
  (* logic *)
  (* !This is really amazing to be able to do just this! *)
  let transformation = combine_stages stages in
  let results = List.map transformation seeds in
  List.fold_left min max_int results

let logic2 (lines : string list) : int =
  (* parsing *)
  let ranges = get_seeds_ranges (List.hd lines) in
  let rev_stages =
    List.tl lines |> List.tl |> split_by_empty_line |> List.rev
    |> List.map get_reverse_stage
  in

  (* logic *)
  let transformation = combine_stages rev_stages in
  let rec loop (i : int) : int =
    let result = transformation i in
    let is_in_range =
      List.exists (fun (s, e) -> result >= s && result <= e) ranges
    in
    if is_in_range then i
    else if i > 100000000 then failwith "too big"
    else loop (i + 1)
  in
  loop 0

let run (path : string) =
  let input = Parse.read path in
  let result = logic2 input in
  print_int result

(* tests - Part 1 *)

let test_input =
  "\n\
   seeds: 79 14 55 13\n\n\
   seed-to-soil map:\n\
   50 98 2\n\
   52 50 48\n\n\
   soil-to-fertilizer map:\n\
   0 15 37\n\
   37 52 2\n\
   39 0 15\n\n\
   fertilizer-to-water map:\n\
   49 53 8\n\
   0 11 42\n\
   42 0 7\n\
   57 7 4\n\n\
   water-to-light map:\n\
   88 18 7\n\
   18 25 70\n\n\
   light-to-temperature map:\n\
   45 77 23\n\
   81 45 19\n\
   68 64 13\n\n\
   temperature-to-humidity map:\n\
   0 69 1\n\
   1 0 69\n\n\
   humidity-to-location map:\n\
   60 56 37\n\
   56 93 4\n"

(* let%test_unit "logic" =   *)
(*   let expected = 35 in *)
(*   [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)

(* let%test_unit "logic" =   *)
(*   let real_input = Parse.read "../inputs/day_5.txt" in *)
(*   let expected =  240320250 in *)
(*   [%test_eq: int] (logic (real_input)) expected *)

(* tests - Part 2 *)

(* let%test_unit "logic" =   *)
(*   let expected = 46 in *)
(*   [%test_eq: int] (logic2 (Parse.get_lines test_input)) expected *)

(* let%test_unit "logic" =   *)
(*   let real_input = Parse.read "../inputs/day_5.txt" in *)
(*   let expected =  28580589 in *)
(*   [%test_eq: int] (logic2 (real_input)) expected *)
