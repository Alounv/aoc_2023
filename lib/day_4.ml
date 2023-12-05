open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

type card = {
  id: string;
  winning: int list;
  owned: int list;
}

let regexp = Str.regexp "[ \t]+"

let get_card (line: string): card = 
  let parts = String.split_on_char ':' line in
  let id = List.hd parts in
  let content = List.nth parts 1 in

  let parts = String.split_on_char '|' content in
  let winning = Str.split (regexp) (List.hd parts) in
  let owned = Str.split (regexp) (List.nth parts 1) in
  let winning = List.map int_of_string winning in
  let owned = List.map int_of_string owned in
  { id; winning; owned }

let get_cards (lines: string list): card list =
  let rec loop (lines: string list) (acc: card list): card list = 
    match lines with
    | [] -> acc
    | line :: lines -> 
      let card = get_card line in
      loop lines (card :: acc)
  in
  loop lines []

let card_score(card: card): int = 
  let rec count = function
    | [] -> 0
    | x :: rest -> 
      if List.mem x card.winning then 1 + count rest
      else count rest
  in
  let win = count card.owned in
  if (win <= 1) then win
  else
    let win = max (win - 1) 0 in
    let score = 2.0 ** (float_of_int win) |> int_of_float in
    score
;;

let logic (lines: string list): int  = 
  let cards = get_cards lines in
  let scores = List.map (card_score) cards in
  List.fold_left (+) 0 scores
;;

let real_input = Parse.read "../inputs/day_4.txt"
let run () = 
  let result = logic real_input in 
  print_int result
;;

(* Part 1 *)

let test_input = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"

let%test_unit "logic" =  
  let expected = 13 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected

let%test_unit "logic" =  
  let expected = 21485 in
  [%test_eq: int] (logic (real_input)) expected

(* Part 2 *)
