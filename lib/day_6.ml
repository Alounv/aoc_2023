open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* utilities *)

let dichotomy (f : int -> bool) (min : int) (max : int) : int =
  let rec aux min max =
    if min = max then min
    else
      let mid = (min + max) / 2 in
      if f mid then aux min mid else aux (mid + 1) max
  in
  aux min max

let dichotomy_backwards (f : int -> bool) (min : int) (max : int) : int =
  let rec aux min max =
    if min = max then min
    else
      let mid = (min + max) / 2 in
      if f mid then aux (mid + 1) max else aux min mid
  in
  aux min max

(* data structures *)

type race = { time : int; distance_record : int }

(* parsing *)

let spaces_reg = Str.regexp "[ \t]+"

let get_values (line : string) : int list =
  Str.split spaces_reg line |> List.tl |> List.map int_of_string

let get_value2 (line : string) : int =
  Str.split spaces_reg line |> List.tl |> String.concat ""
  |> Str.global_replace spaces_reg ""
  |> int_of_string

let get_games (input : string list) : race list =
  let times = input |> List.hd |> get_values in
  let dists = input |> List.tl |> List.hd |> get_values in
  List.map2 (fun t d -> { time = t; distance_record = d }) times dists

let get_game2 (input : string list) : race =
  let time = input |> List.hd |> get_value2 in
  let dist = input |> List.tl |> List.hd |> get_value2 in
  { time; distance_record = dist }

(* logic *)

let get_distance (prep : int) (time : int) : int = prep * (time - prep)

let is_above (race : race) (prep : int) : bool =
  if prep < 0 || prep > race.time then false
  else
    let distance = get_distance prep race.time in
    distance > race.distance_record

let first_ok_prep_time (race : race) : int =
  dichotomy (is_above race) 0 race.time

let last_ok_prep_time (race : race) : int =
  dichotomy_backwards (is_above race) 0 race.time - 1

let get_solutions_count (race : race) : int =
  let first = first_ok_prep_time race in
  let last = last_ok_prep_time race in
  last - first + 1

let get_speed_from_preparation (preparation : int) (time : int) : int =
  preparation * time

let logic input =
  get_games input
  |> List.map get_solutions_count
  |> List.fold_left (fun acc x -> acc * x) 1

let logic2 input = get_game2 input |> get_solutions_count

(* main *)

let run (path : string) =
  let input = Parse.read path in
  let result = logic input in
  print_int result

(* tests - Part 1 *)

let test_input = "\nTime:      7  15   30\nDistance:  9  40  200\n"

let%test_unit "logic" =
  let expected = 288 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_6.txt" in
  let expected = 2269432 in
  [%test_eq: int] (logic real_input) expected

(* tests - Part 1 *)

let%test_unit "logic" =
  let expected = 71503 in
  [%test_eq: int] (logic2 (Parse.get_lines test_input)) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_6.txt" in
  let expected = 35865985 in
  [%test_eq: int] (logic2 real_input) expected
