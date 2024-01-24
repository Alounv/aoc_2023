(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open Utilities
open List

(* data structures *)

type node_description = { name : string; left : string; right : string }
type instruction = L | R
type event = { start : int; period : int }
type max = { max_start : int }

(* parsing *)

let get_instructions (lines : string list) : instruction list =
  lines |> List.hd |> String.to_seq |> List.of_seq
  |> List.map (fun c ->
      match c with
      | 'L' -> L
      | 'R' -> R
      | _ -> failwith "invalid instruction")

let rec parse_nodes (lines : string list) : node_description list =
  match lines with
  | [] -> []
  | hd :: tl ->
    let parts = String.split_on_char ' ' hd in
    let name = List.hd parts in
    let left = List.nth parts 2 in
    let left = String.sub left 1 3 in
    let right = List.nth parts 3 in
    let right = String.sub right 0 3 in
    { name; left; right } :: parse_nodes tl

let get_nodes_list (lines : string list) : node_description list =
  let test = lines |> List.tl |> List.tl in
  test |> parse_nodes

(* logic 1 *)

let rec move (nodes : node_description list) (instructions : instruction list)
    (index : int) (current : string) : int =
  match current with
  | "ZZZ" -> index
  | _ -> (
      let used_index = index mod List.length instructions in
      let dir = List.nth instructions used_index in
      let node = List.find (fun n -> n.name = current) nodes in
      match dir with
      | L -> move nodes instructions (index + 1) node.left
      | R -> move nodes instructions (index + 1) node.right)

let logic (lines : string list) : int =
  let instructions = lines |> get_instructions in
  let nodes = lines |> get_nodes_list in
  move nodes instructions 0 "AAA"

(* logic 2 *)

let rec moveToZ (nodes : node_description list)
    (instructions : instruction list) (index : int) (current : string)
    (first : int) : int * int =
  if current.[2] = 'Z' && first > 0 then (first, index)
  else
    let first =
      if first > 0 then first else if current.[2] = 'Z' then index else 0
    in
    let used_index = index mod List.length instructions in
    let dir = List.nth instructions used_index in
    let node = List.find (fun n -> n.name = current) nodes in
    match dir with
    | L -> moveToZ nodes instructions (index + 1) node.left first
    | R -> moveToZ nodes instructions (index + 1) node.right first

let get_first_and_second_index (nodes : node_description list)
    (instructions : instruction list) (index : int) (current : string) :
  int * int =
  let first_index, second_index = moveToZ nodes instructions index current 0 in
  (first_index, second_index)

let get_starting_names (nodes : node_description list) : string list =
  nodes |> List.map (fun n -> n.name) |> List.filter (fun n -> n.[2] = 'A')

let get_first_simultaneous_index (events : event list) : int =
  let periods = events |> List.map (fun e -> e.period) in
  let lcm = lcm_list periods in
  lcm

let logic2 (lines : string list) : int =
  let instructions = lines |> get_instructions in
  let nodes = lines |> get_nodes_list in
  let starting_names = nodes |> get_starting_names in
  let paths =
    starting_names
    |> List.map (fun n -> get_first_and_second_index nodes instructions 0 n)
  in
  let events = List.map (fun (a, b) -> { start = a; period = b - a }) paths in
  get_first_simultaneous_index events

(* main *)

let run (path : string) =
  let input = Parse.read path in
  let result = logic2 input in
  print_int result

(* (* tests - Part 1 *) *)
(**)
(* let test_input = *)
(*   "\nLLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)\n" *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 6 in *)
(*   [%test_eq: int] (logic (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_8.txt" in *)
(*   let expected = 18827 in *)
(*   [%test_eq: int] (logic real_input) expected *)
(**)
(* (* tests - Part 2 *) *)
(**)
(* let test_input = *)
(* "\n\ *)
   (*    LR\n\n\ *)
   (*    11A = (11B, XXX)\n\ *)
   (*    11B = (XXX, 11Z)\n\ *)
   (*    11Z = (11B, XXX)\n\ *)
   (*    22A = (22B, XXX)\n\ *)
   (*    22B = (22C, 22C)\n\ *)
   (*    22C = (22Z, 22Z)\n\ *)
   (*    22Z = (22B, 22B)\n\ *)
   (*    XXX = (XXX, XXX)\n" *)
(**)
(* let%test_unit "logic" = *)
(*   let expected = 6 in *)
(*   [%test_eq: int] (logic2 (Parse.get_lines test_input)) expected *)
(**)
(* let%test_unit "logic" = *)
(*   let real_input = Parse.read "../inputs/day_8.txt" in *)
(*   let expected = 20220305520997 in *)
(*   [%test_eq: int] (logic2 real_input) expected *)
