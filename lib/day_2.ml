open Ppx_compare_lib.Builtin
open Sexplib.Std
open List


type  rgb = {
  red: int;
  green: int;
  blue: int;
}

let parse_game (game: string) : rgb= 
  let game = String.trim game in
  let colors = String.split_on_char ',' game in
  let colors = List.map String.trim colors in
  let colors = List.map (String.split_on_char ' ') colors in
  let colors = List.map (fun x -> (int_of_string (List.nth x 0), List.nth x 1)) colors in

  let colors = List.map (fun c -> 
      let color = List.find_opt (fun x -> snd x = c) colors in
      match color with
      | Some x -> fst x
      | None -> 0
    ) ["red"; "green"; "blue"]
  in

  { red = List.nth colors 0; green = List.nth colors 1; blue = List.nth colors 2}
;;

type line = {
  id: int;
  max: rgb;
}

let get_line_max (line: string) : line =
  let line = String.split_on_char ':' line in
  let id = List.nth line 0 in
  let id = String.split_on_char ' ' id in
  let id = List.nth id 1 in

  let games =  List.nth line 1 in
  let games = String.split_on_char ';' games in
  let games = List.map parse_game games in

  let max = List.fold_left (fun acc x -> {
        red = max acc.red x.red;
        green = max acc.green x.green;
        blue = max acc.blue x.blue
      }) {red = 0; green = 0; blue = 0} games in

  {id = int_of_string id; max = max}
;;

let logic (lines: string list) (input: rgb) : int  = 
  let lines = List.map get_line_max lines in
  let lines = List.filter (fun x -> x.max.red <= input.red && x.max.green <= input.green && x.max.blue <= input.blue) lines in
  let sum_of_ids = List.fold_left (fun acc x -> acc + x.id) 0 lines in
  sum_of_ids
;;

let real_input = Parse.read "../inputs/day_2.txt"
let run () = 
  let result = logic real_input {red = 12; green = 13; blue = 14} in
  print_int result
;;

(* Part 1 *)

let test_rgb = {red = 12; green = 13; blue = 14}
let test_input = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"
let expected = 8
let%test_unit "logic" = [%test_eq: int] (logic (Parse.get_lines test_input) test_rgb) expected

let expected = 2176
let%test_unit "logic" = [%test_eq: int] (logic real_input test_rgb) expected
