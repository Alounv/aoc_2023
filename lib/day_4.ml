(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List

type card = {
  id: int;
  winning: int list;
  owned: int list;
}

let spaces_reg = Str.regexp "[ \t]+"

let get_card (line: string): card = 
  let parts = String.split_on_char ':' line in
  let id = List.hd parts in
  let id = Str.split (spaces_reg) (id) in
  let id = List.nth id 1 in
  let id = int_of_string id in
  let content = List.nth parts 1 in

  let parts = String.split_on_char '|' content in
  let winning = Str.split (spaces_reg) (List.hd parts) in
  let owned = Str.split (spaces_reg) (List.nth parts 1) in
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

let card_win(card: card): int = 
  let rec count = function
    | [] -> 0
    | x :: rest -> 
      if List.mem x card.winning then 1 + count rest
      else count rest
  in
  count card.owned 
;;

let card_score(card: card): int = 
  let win = card_win card in
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

let get_won_cards (id: int) (win: int): int list = 
  let rec loop (id: int) (win: int) (acc: int list): int list = 
    if win = 0 then acc
    else loop (id + 1) (win - 1) (id :: acc)
  in
  loop (id + 1) win []
;;

module M = Map.Make(Int)

let increment_value (count_map: int M.t) (cardId: int) (increment: int): int M.t = 
  let value = M.find_opt cardId count_map in
  match value with
  | None -> M.add cardId increment count_map
  | Some value -> M.add cardId (value + increment) count_map

let rec increment_card_values (count_map: int M.t) (cardsIds: int list) (increment: int): int M.t = 
  match cardsIds with
  | [] -> count_map
  | cardId :: rest -> 
    let count_map = increment_value count_map cardId increment in
    increment_card_values count_map rest increment
;;

let rec get_scratched_card (wins_map: int M.t) (count_map: int M.t) (index: int): int M.t = 
  let cards_count = M.cardinal wins_map in
  if index > cards_count then count_map
  else
    let count_map = increment_value count_map index 1 in
    let current_value = M.find index count_map in
    let wins = M.find index wins_map in
    let won_cards = get_won_cards index wins in
    let count_map = increment_card_values count_map won_cards current_value in
    get_scratched_card wins_map count_map (index + 1)
;;

let logic2 (lines: string list): int  = 
  let cards = get_cards lines in
  let cards_wins = List.map (fun card -> (card.id, card_win card)) cards in
  let wins_map = List.fold_left (fun acc (id, win) -> M.add id win acc) M.empty cards_wins in
  let get_values = get_scratched_card wins_map in
  let count_map = get_values M.empty 1 in
  let sum = M.fold (fun _ value acc -> acc + value) count_map 0 in
  sum
;;


let run (path: string) = 
  let input = Parse.read path in
  let result = logic2 input in 
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


(* let%test_unit "logic" =   *)
(*   let expected = 13 in *)
(*   [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic" =   *)
(*   let expected = 21485 in *)
(*   [%test_eq: int] (logic (real_input)) expected *)

(* Part 2 *)

(* let%test_unit "logic2" =   *)
(*   let expected = 30 in *)
(*   [%test_eq: int] (logic2 (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic2" =   *)
(*   let real_input = Parse.read "../inputs/day_4.txt" in *)
(*   let expected = 11024379 in *)
(*   [%test_eq: int] (logic2 (real_input)) expected *)
