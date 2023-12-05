open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* data structures *)

type transform = {
  s: int;
  e: int;
  inc: int;
}

type stage = {
  id: string;
  transforms: transform list;
}


(* parsing *)

let get_seeds (line: string): int list = 
  let parts = String.split_on_char ':' line in
  let seeds = List.nth parts 1 
              |> String.trim
              |> String.split_on_char ' '
              |> List.map int_of_string
  in seeds
;;

let get_transform (line: string): transform = 
  let parts = String.split_on_char ' ' line in
  let values = List.map int_of_string parts in
  let destination = List.nth values 0 in
  let s = List.nth values 1 in
  let range = List.nth values 2 in
  {
    s = s;
    e = (s + range);
    inc = destination - s;
  }
;;

let get_stage (lines: string list): stage = 
  let id = List.nth lines 0 in
  let transforms = List.map get_transform (List.tl lines) in
  {
    id = id;
    transforms = transforms;
  }
;;

let split_by_empty_line (lines: string list): string list list = 
  let rec loop (lines: string list) (stages: string list list): string list list = 
    match lines with
    | [] -> stages
    | ""::rest -> loop rest ([]::stages)
    | line::rest_lines -> 
      let c_stage = List.hd stages in
      let rest_stages = List.tl stages in 
      let updated_stage = line::c_stage in
      loop rest_lines (updated_stage::rest_stages)
  in 
  loop lines [[]]
  |> List.map List.rev
  |> List.rev

(* logic *)

let transform (transforms: transform list) (origin: int): int = 
  let rec loop (transforms: transform list) (origin: int): int = 
    match transforms with
    | [] -> origin
    | {s;e;inc}::rest -> 
      if origin >= s && origin <= e then 
        (origin + inc)
      else 
        loop rest origin
  in loop transforms origin 
;;

let combine_stages (stages: stage list) (origin: int): int = 
  let rec loop (stages: stage list) (origin: int): int = 
    match stages with
    | [] -> origin
    | stage::rest -> 
      let origin = transform stage.transforms origin in
      loop rest origin
  in loop stages origin
;;

(* main *)

let logic (lines: string list): int  = 
  (* parsing *)
  let seeds = get_seeds (List.hd lines) in
  let stages_lines = List.tl (List.tl lines) in
  let stages = split_by_empty_line (stages_lines) in
  let stages = List.map get_stage stages in

  (* logic *)
  (* !This is really amazing to be able to do just this! *)
  let transformation = combine_stages stages in
  let results = List.map transformation seeds in
  List.fold_left min max_int results 
;;

let run (path: string) = 
  let input = Parse.read path in
  let result = logic input in 
  print_int result
;;


(* tests - Part 1 *)

let test_input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"


let%test_unit "logic" =  
  let expected = 35 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected
