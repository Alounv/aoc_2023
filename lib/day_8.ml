open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* utilities *)

(* data structures *)

type node_description = { name : string; left : string; right : string }
type instruction = L | R

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

(* logic *)

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

(* main *)

let run (path : string) =
  let input = Parse.read path in
  let result = logic input in
  print_int result

(* tests - Part 1 *)

let test_input =
  "\nLLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)\n"

let%test_unit "logic" =
  let expected = 6 in
  [%test_eq: int] (logic (Parse.get_lines test_input)) expected

let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_8.txt" in
  let expected = 18827 in
  [%test_eq: int] (logic real_input) expected

(* tests - Part 2 *)
