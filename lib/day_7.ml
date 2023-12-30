open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* utilities *)

(* data structures *)

type repartition = (char, int) Hashtbl.t

type figures =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPairs
  | OnePair
  | HighCard

(* parsing *)

let get_hand_bid (line : string) : char list * int =
  let split = String.split_on_char ' ' line in
  let hand = split |> hd |> String.to_seq |> List.of_seq in
  let bid = split |> tl |> hd |> int_of_string in
  (hand, bid)

let get_repartition (hand : char list) : repartition =
  let rec aux (hand : char list) (acc : repartition) : repartition =
    match hand with
    | [] -> acc
    | h :: _ ->
        let previous = Hashtbl.find_opt acc h |> Option.value ~default:0 in
        Hashtbl.replace acc h (previous + 1);
        aux (tl hand) acc
  in
  aux hand (Hashtbl.create 5)

let get_max_char (repartition : repartition) : char =
  let sorted_repartition =
    repartition |> Hashtbl.to_seq |> List.of_seq
    |> List.sort (fun (_, v1) (_, v2) -> v2 - v1)
  in
  sorted_repartition |> hd |> fst

let correct_repartition_with_jokers (repartition : repartition) : repartition =
  let jokers_count =
    Hashtbl.find_opt repartition 'J' |> Option.value ~default:0
  in
  let len = Hashtbl.length repartition in
  let max_char = get_max_char repartition in

  if jokers_count = 0 || (len == 1 && max_char = 'J') then repartition
  else
    let () = Hashtbl.remove repartition 'J' in
    let next_max_char = get_max_char repartition in
    let max_char = if max_char = 'J' then next_max_char else max_char in

    let max_char_value =
      Hashtbl.find_opt repartition max_char |> Option.value ~default:0
    in
    Hashtbl.replace repartition max_char (jokers_count + max_char_value);
    repartition

(* logic *)

let get_highest_figure (repartition : repartition) : figures =
  let entries = repartition |> Hashtbl.to_seq |> List.of_seq in
  let sorted_entries = List.sort (fun (_, v1) (_, v2) -> v2 - v1) entries in
  let first = hd sorted_entries in

  match snd first with
  | 5 -> FiveOfAKind
  | 4 -> FourOfAKind
  | 3 -> (
      match sorted_entries |> tl |> hd |> snd with
      | 2 -> FullHouse
      | _ -> ThreeOfAKind)
  | 2 -> (
      match sorted_entries |> tl |> hd |> snd with
      | 2 -> TwoPairs
      | _ -> OnePair)
  | 1 -> HighCard
  | _ -> failwith "invalid figure"

let get_figure_value (figure : figures) : int =
  match figure with
  | FiveOfAKind -> 60
  | FourOfAKind -> 50
  | FullHouse -> 40
  | ThreeOfAKind -> 30
  | TwoPairs -> 20
  | OnePair -> 10
  | HighCard -> 0

let get_card_value (card : char) : int =
  match card with
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'T' -> 10
  | 'J' -> 1
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> failwith "invalid card"

let get_hands_values (hands : (char list * int) list) :
    (char list * int * int) list =
  let rec aux (hands : (char list * int) list)
      (acc : (char list * int * int) list) : (char list * int * int) list =
    match hands with
    | [] -> acc
    | (hand, bid) :: tl ->
        let repartition = get_repartition hand in
        let new_repartition = correct_repartition_with_jokers repartition in
        let figure = get_highest_figure new_repartition in
        let value = get_figure_value figure in
        aux tl ((hand, value, bid) :: acc)
  in
  aux hands []

let compare_cards (char1 : char) (char2 : char) : int =
  let card1 = char1 |> get_card_value in
  let card2 = char2 |> get_card_value in
  card2 - card1

let compare_hands_by_cards (hand1 : char list) (hand2 : char list) : int =
  let rec aux (hand1 : char list) (hand2 : char list) : int =
    match (hand1, hand2) with
    | [], [] -> 0
    | h1 :: t1, h2 :: t2 -> if h1 = h2 then aux t1 t2 else compare_cards h1 h2
    | _ -> failwith "invalid hands"
  in
  aux hand1 hand2

let compare_hands (hand1 : char list * int * int)
    (hand2 : char list * int * int) : int =
  let hand1, value1, _ = hand1 in
  let hand2, value2, _ = hand2 in
  if value1 = value2 then compare_hands_by_cards hand1 hand2
  else value2 - value1

let add_index_to_bid (bids : int list) : (int * int) list =
  let rec aux (bids : int list) (acc : (int * int) list) : (int * int) list =
    match bids with [] -> acc | h :: t -> aux t ((h, length t) :: acc)
  in
  aux bids []

(* main *)

let logic (input : string list) : int =
  let hands = map get_hand_bid input in
  let hands_values = get_hands_values hands in
  let sorted_hands = List.sort compare_hands hands_values in
  let sorted_bids = sorted_hands |> map (fun (_, _, bid) -> bid) in

  let ranked_bids = add_index_to_bid sorted_bids in
  let result =
    ranked_bids
    |> map (fun (bid, rank) -> bid * (rank + 1))
    |> fold_left ( + ) 0
  in

  result

let run (path : string) =
  let input = Parse.read path in
  let result = logic input in
  print_int result

(* tests - Part 1 *)

let test_input = "\n32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483\n"

(* let%test_unit "logic" = *)
(*   let expected = 6440 in *)
(*   [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_7.txt" in *)
(*   let expected = 248422077 in *)
(*   [%test_eq: int] (logic real_input) expected *)

(* tests - Part 2 *)

let%test_unit "logic" =
  let expected = 5905 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_7.txt" in
  let expected = 249817836 in
  [%test_eq: int] (logic real_input) expected
