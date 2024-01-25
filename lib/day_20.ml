(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List
open Str
open Utilities

(* ============== TYPES ============== *)

type mod_types = C | FF | B | None

type box = {
  d : string list;
  t : mod_types;
  on : bool;
  mem : string list;
  conn : int;
}
[@@deriving sexp]

type boxes = (string, box) Hashtbl.t
type signal_value = L | H
type signal = { dest : string; value : signal_value; origin : string }

let print_box (box : box) : unit =
  Printf.printf "%s [%s] - %s - %s - conn: %d\n"
    (match box.t with C -> "C" | FF -> "FF" | B -> "B" | None -> "None")
    (String.concat ", " box.d)
    (if box.on then "O" else " ")
    (String.concat ", " box.mem)
    box.conn

let print_boxes (box : box list) : unit = List.iter print_box box

let print_hash_boxes (box : boxes) : unit =
  Hashtbl.iter
    (fun k v ->
       Printf.printf "%s: " k;
       print_box v)
    box;
  Printf.printf "\n"

let print_signal (signal : signal) : unit =
  Printf.printf "%s -%s-> %s\n" signal.origin
    (match signal.value with L -> "L" | H -> "H")
    signal.dest

let print_signals (signals : signal list) : unit =
  List.iter print_signal signals;
  Printf.printf "\n"

let get_box_hash (name : string) (b : box) : string =
  let mem = List.sort (fun a b -> String.compare a b) b.mem in
  Printf.sprintf "[%s-%s-%s]" name
    (if b.on then "O" else "")
    (if List.length mem > 0 then String.concat "," mem else "")

let get_boxes_hash (boxes : boxes) : string =
  let boxes : (string * box) list =
    boxes |> Hashtbl.to_seq |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  boxes |> List.map (fun (name, b) -> get_box_hash name b) |> String.concat " "

(* ============== INPUT ============== *)

let test_input_1 =
  Parse.get_lines
    "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"

let test_input_2 =
  Parse.get_lines
    "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"

(* ============== PARSING ============== *)

let get_type (str : string) : mod_types =
  match str with
  | "&" -> C
  | "%" -> FF
  | "broadcaster" -> B
  | _ -> failwith "Unknown type"

let parse_line (line : string) : string * box =
  let parts = Str.split (Str.regexp " -> ") line in

  (* Get name and type *)
  let name = List.hd parts in
  let first_char = String.get name 0 in
  let t =
    match first_char with
    | '&' -> C
    | '%' -> FF
    | _ -> if name = "broadcaster" then B else None
  in
  let name =
    match t with
    | C | FF -> String.sub name 1 (String.length name - 1)
    | _ -> name
  in

  (* Get destinations *)
  let dest = List.nth parts 1 in
  let d = dest |> Str.split (regexp ", ") in
  (name, { d; t; on = false; mem = []; conn = 0 })

(* ============== LOGIC DAY 1 ============== *)

let add_connection (boxes : (string * box) list) : (string * box) list =
  let add_connection (name : string) (box : box) : box =
    let conn =
      boxes |> List.filter (fun (_, b) -> List.mem name b.d) |> List.length
    in
    { box with conn }
  in

  boxes
  |> List.map (fun (n, b) ->
      if b.t = C then (n, add_connection n b) else (n, b))

let dispatch_signal (value : signal_value) (origin : string)
    (dest : string list) : signal list =
  List.map (fun d -> { origin; dest = d; value }) dest

let get_box_signals (box : box) (incoming : signal) : box * signal list =
  let option =
    match box.t with
    | FF -> (
        match incoming.value with
        | L ->
          let box = { box with on = not box.on } in
          let emitted = if box.on then H else L in
          Some (box, emitted)
        | H -> None)
    | C ->
      let mem = box.mem |> List.filter (fun x -> x <> incoming.origin) in
      let mem = if incoming.value = H then incoming.origin :: mem else mem in
      if List.length mem > box.conn then failwith "Too many";
      if List.length mem = box.conn then Some ({ box with mem }, L)
      else Some ({ box with mem }, H)
    | B -> Some (box, incoming.value)
    | None -> None
  in

  match option with
  | None -> (box, [])
  | Some (box, emitted) -> (box, dispatch_signal emitted incoming.dest box.d)

let step (boxes : boxes) : int * int * boxes =
  let boxes = boxes in
  let queue : signal list ref =
    ref [ { dest = "broadcaster"; value = L; origin = "start" } ]
  in
  let low = ref 0 in
  let high = ref 0 in

  while List.length !queue > 0 do
    let signal = List.hd !queue in
    if signal.value = L then low := !low + 1 else high := !high + 1;

    queue := List.tl !queue;

    let box = Hashtbl.find_opt boxes signal.dest in
    match box with
    | None -> ()
    | Some box ->
      let box, signals = get_box_signals box signal in
      Hashtbl.replace boxes signal.dest box;

      (* we put them at the end of the queue *)
      queue := !queue @ signals
  done;
  (!low, !high, boxes)

let get_regularity (initial_boxes : boxes) (max : int) : int * int * int * bool
  =
  let initial_hash = get_boxes_hash initial_boxes in
  let boxes = ref initial_boxes in
  let count = ref 0 in
  let low = ref 0 in
  let high = ref 0 in
  let has_pattern = ref false in

  Printf.printf "initial hash:\n %s\n\n" initial_hash;
  while
    !count <= max && (!count = 0 || initial_hash <> get_boxes_hash !boxes)
  do
    count := !count + 1;
    let l, h, b = step !boxes in
    low := !low + l;
    high := !high + h;
    boxes := b
    (* Printf.printf "hash:\n %s\n\n" (get_boxes_hash !boxes) *)
  done;

  if !count < max + 1 then has_pattern := true;

  (!low, !high, !count, !has_pattern)

let logic1 (input : string t) : int =
  let boxes = List.map parse_line input in
  let boxes_list = boxes |> add_connection |> List.to_seq in
  let clone_1 = boxes_list |> Hashtbl.of_seq in
  let clone_2 = boxes_list |> Hashtbl.of_seq in
  let low = ref 0 in
  let high = ref 0 in
  let max = 1000 in

  let l, h, r, has_pattern = get_regularity clone_1 max in
  Printf.printf "l: %d, h: %d, r: %d, has_pattern: %b\n" l h r has_pattern;
  if has_pattern then (
    let m = max / r in
    low := m * l;
    high := m * h);

  let rest = max mod r in
  for _ = 0 to rest - 1 do
    let l, h, _ = step clone_2 in
    low := !low + l;
    high := !high + h
  done;

  !low * !high

(* ============== LOGIC DAY 2 ============== *)

let step2 (boxes : boxes) (prev_boxes : string t) : string =
  let queue : signal list ref =
    ref [ { dest = "broadcaster"; value = L; origin = "start" } ]
  in
  let result = ref "" in

  while List.length !queue > 0 && String.length !result = 0 do
    let signal = List.hd !queue in
    if signal.value = L && List.mem signal.dest prev_boxes then
      result := signal.dest
    else (
      queue := List.tl !queue;
      let box = Hashtbl.find_opt boxes signal.dest in
      match box with
      | None -> ()
      | Some box ->
        let box, signals = get_box_signals box signal in
        Hashtbl.replace boxes signal.dest box;
        queue := !queue @ signals)
  done;

  !result

let logic2 (input : string t) : int =
  let boxes = List.map parse_line input in
  let boxes_list = boxes |> add_connection in
  let boxes_seq = boxes_list |> List.to_seq in
  let boxes = boxes_seq |> Hashtbl.of_seq in

  let final = "rx" in
  let prev, _ =
    boxes_list |> List.find (fun (_, box) -> List.mem final box.d)
  in
  let prev_boxes =
    boxes_list
    |> List.filter (fun (_, box) -> List.mem prev box.d)
    |> List.map fst
  in
  let prev_boxes_num = Hashtbl.create (List.length prev_boxes) in
  let prev_boxes = ref prev_boxes in
  let i = ref 0 in

  while List.length !prev_boxes > 0 do
    i := !i + 1;
    let res = step2 boxes !prev_boxes in
    if res <> "" then
      if not (Hashtbl.mem prev_boxes_num res) then (
        Hashtbl.replace prev_boxes_num res !i;
        prev_boxes := !prev_boxes |> List.filter (fun x -> x <> res))
  done;

  let prev_boxes_list = prev_boxes_num |> Hashtbl.to_seq |> List.of_seq in

  prev_boxes_list |> List.iter (fun (k, v) -> Printf.printf "%s: %d\n" k v);

  (* combination of prev_boxes_num to the least common multiple *)
  let lcm = lcm_list (List.map snd prev_boxes_list) in

  lcm

(* ============== RUN ============== *)

let run (_path : string) =
  (* let input = Parse.read path |> String.concat "\n" in *)
  let input = test_input_1 in
  let result = logic1 input in
  Printf.printf "%d\n" result

(* ============== TESTS DAY 1 ============== *)

(* let%test_unit "logic1" = *)
(*   let expected = 32000000 in *)
(*   [%test_eq: int] (logic1 test_input_1) expected *)
(**)
(* let%test_unit "logic1" = *)
(*   let expected = 11687500 in *)
(*   [%test_eq: int] (logic1 test_input_2) expected *)
(**)
(* let%test_unit "logic1" = *)
(*   let real_input = Parse.read "../inputs/day_20.txt" in *)
(*   let expected = 898557000 in *)
(*   [%test_eq: int] (logic1 real_input) expected *)

(* ============== LOGIC DAY 2 ============== *)

(* let%test_unit "logic2" = *)
(*   let real_input = Parse.read "../inputs/day_20.txt" in *)
(*   let expected = 238420328103151 in *)
(*   [%test_eq: int] (logic2 real_input) expected *)
