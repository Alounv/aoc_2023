open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
(* open Utilities *)

(* data structures *)

type lens = { label : string; focal_length : int }
type box = lens list
type boxes = (int, box) Hashtbl.t
type operation = Add | Remove

(* utilities *)

let print_lens (lens : lens) : string =
  Printf.sprintf "[%s %d]" lens.label lens.focal_length

let print_box (box : box) (index : int) : string =
  let lenses = box |> List.map print_lens |> String.concat " " in
  Printf.sprintf "Box %d: %s" index lenses

let print_boxes (boxes : boxes) : string =
  let boxes = Hashtbl.to_seq boxes |> List.of_seq in
  let boxes =
    boxes
    |> List.map (fun (index, box) -> print_box box index)
    |> String.concat "\n"
  in
  Printf.sprintf "%s\n" boxes

let draw (boxes : boxes) : unit = Printf.printf "%s\n" (print_boxes boxes)

(* parsing *)

(* input *)

let test_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

(* hash *)

let fold (acc : int) (value : int) : int =
  let acc = acc + value in
  let acc = acc * 17 in
  acc mod 256

let hash (line : string) : int =
  let chars = line |> String.to_seq |> List.of_seq in
  let values = chars |> List.map Char.code in
  values |> List.fold_left fold 0

(* logic 1 *)

let logic1 (input : string) : int =
  let lines = Parse.get_lines_from_sep input ',' in
  let values = lines |> List.map hash in
  values |> List.fold_left ( + ) 0

(* focusing power *)

let lens_focusing_power (box_index : int) (lens_index : int)
    (focal_length : int) : int =
  (box_index + 1) * (lens_index + 1) * focal_length

let box_focusing_power (box_index : int) (box : box) : int =
  let lenses =
    box
    |> List.mapi (fun i lens ->
        lens_focusing_power box_index i lens.focal_length)
  in
  List.fold_left ( + ) 0 lenses

(* logic 2 *)

let remove (lenses : lens list) (label : string) : lens list =
  let lenses = lenses |> List.filter (fun lens -> lens.label <> label) in
  lenses

let add (lenses : lens list) (label : string) (focal_length : int) : lens list =
  lenses @ [ { label; focal_length } ]

let replace (lenses : lens list) (label : string) (focal_length : int) :
  lens list =
  let new_lens = { label; focal_length } in
  List.map (fun lens -> if lens.label = label then new_lens else lens) lenses

let add_or_replace (lenses : lens list) (label : string) (focal_length : int) :
  lens list =
  let exists = List.exists (fun lens -> lens.label = label) lenses in
  if exists then replace lenses label focal_length
  else add lenses label focal_length

let update (lenses : lens list) (label : string) (focal_length : int)
    (op : operation) : lens list =
  match op with
  | Add -> add_or_replace lenses label focal_length
  | Remove -> remove lenses label

type instruction = { label : string; operation : operation; focal_length : int }

let apply_instruction (boxes : boxes) (i : instruction) : unit =
  let box_index = hash i.label in

  if not (Hashtbl.mem boxes box_index) then Hashtbl.add boxes box_index [];
  let lens = Hashtbl.find boxes box_index in
  let lens = update lens i.label i.focal_length i.operation in

  if lens = [] then Hashtbl.remove boxes box_index
  else Hashtbl.replace boxes box_index lens;

  ()

(* parsing 2 *)

let get_instruction (line : string) : instruction =
  let sep_index = String.index_opt line '=' in
  let sep_index =
    match sep_index with Some index -> index | None -> String.index line '-'
  in
  let char = line.[sep_index] in
  let operation = if char = '=' then Add else Remove in
  let parts = String.split_on_char char line in
  let label = List.nth parts 0 in
  if operation = Remove then { label; operation; focal_length = 0 }
  else
    let focal_length = List.nth parts 1 in
    let focal_length = int_of_string focal_length in
    { label; operation; focal_length }

let get_instructions (lines : string t) : instruction list =
  List.map get_instruction lines

let fill (boxes : boxes) (instructions : instruction list) : unit =
  List.iter (apply_instruction boxes) instructions

let logic2 (input : string) : int =
  let lines = Parse.get_lines_from_sep input ',' in
  let instructions = get_instructions lines in
  let hashmap = Hashtbl.create 256 in
  fill hashmap instructions;

  let boxes = Hashtbl.to_seq hashmap |> List.of_seq in
  let boxes =
    List.map (fun (index, box) -> box_focusing_power index box) boxes
  in
  List.fold_left ( + ) 0 boxes

(* main *)

let run (_path : string) =
  (* let input = Parse.read path in *)
  let load = logic1 test_input in
  Printf.printf "%d\n" load

(* tests - Part 1 *)

let%test_unit "hash" = [%test_eq: int] (hash "HASH") 52

let%test_unit "logic1" =
  let expected = 1320 in
  [%test_eq: int] (logic1 test_input) expected

let%test_unit "logic1" =
  let real_input = Parse.read "../inputs/day_15.txt" |> List.hd in
  let expected = 506269 in
  [%test_eq: int] (logic1 real_input) expected

(* tests - Part 2 *)

let%test_unit "apply_instructions" =
  let instructions =
    [
      { label = "rn"; operation = Add; focal_length = 1 };
      { label = "cm"; operation = Remove; focal_length = 0 };
      { label = "qp"; operation = Add; focal_length = 3 };
      { label = "cm"; operation = Add; focal_length = 2 };
      { label = "qp"; operation = Remove; focal_length = 0 };
      { label = "pc"; operation = Add; focal_length = 4 };
      { label = "ot"; operation = Add; focal_length = 9 };
      { label = "ab"; operation = Add; focal_length = 5 };
      { label = "pc"; operation = Remove; focal_length = 0 };
      { label = "pc"; operation = Add; focal_length = 6 };
      { label = "ot"; operation = Add; focal_length = 7 };
    ]
  in
  let boxes = Hashtbl.create 256 in
  fill boxes instructions;
  let result = print_boxes boxes in
  let expected = "Box 0: [rn 1] [cm 2]\nBox 3: [ot 7] [ab 5] [pc 6]\n" in
  [%test_eq: string] result expected

let%test_unit "logic2" =
  let expected = 145 in
  [%test_eq: int] (logic2 test_input) expected

let%test_unit "logic2" =
  let real_input = Parse.read "../inputs/day_15.txt" |> List.hd in
  let expected = 264021 in
  [%test_eq: int] (logic2 real_input) expected
