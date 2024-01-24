(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List
open Str
(* open Utilities *)

(* ============== TYPES ============== *)

type operator = Lt | Gt
type comparison = { prop : string; op : operator; value : int }
type operation = { comp : comparison; next : string }
type workflow = { name : string; ops : operation t }
type part = { x : int; m : int; a : int; s : int }
type workflows = (string, workflow) Hashtbl.t

(* ============== INPUT ============== *)

let test_input =
  "px{a<2006:qkq,m>2090:A,rfg}\n\
   pv{a>1716:R,A}\n\
   lnx{m>1548:A,A}\n\
   rfg{s<537:gd,x>2440:R,A}\n\
   qs{s>3448:A,lnx}\n\
   qkq{x<1416:A,crn}\n\
   crn{x>2662:A,R}\n\
   in{s<1351:px,qqz}\n\
   qqz{s>2770:qs,m<1801:hdj,R}\n\
   gd{a>3333:R,R}\n\
   hdj{m>838:A,pv}\n\n\
   {x=787,m=2655,a=1222,s=2876}\n\
   {x=1679,m=44,a=2067,s=496}\n\
   {x=2036,m=264,a=79,s=2244}\n\
   {x=2461,m=1339,a=466,s=291}\n\
   {x=2127,m=1623,a=2188,s=1013}"

(* ============== PARSING ============== *)

let parse_comparison str =
  let regex = regexp "\\([a-zA-Z]+\\)\\(<\\|>\\|=\\)\\([0-9]+\\)" in
  if string_match regex str 0 then
    let prop = matched_group 1 str in
    let operator_str = matched_group 2 str in
    let value = int_of_string (matched_group 3 str) in
    let op =
      match operator_str with
      | "<" -> Lt
      | ">" -> Gt
      | _ -> failwith "Invalid operator"
    in
    { prop; op; value }
  else failwith "Invalid comparison"

let parse_operation str =
  let op = split (regexp ":") str in
  if length op = 1 then
    let next = List.hd op in
    (* should always pass *)
    { comp = { prop = "x"; op = Gt; value = -1 }; next }
  else
    let comp = parse_comparison (List.hd op) in
    let next = List.nth op 1 in
    { comp; next }

let parse_part str =
  let regex =
    regexp "{x=\\([0-9]+\\),m=\\([0-9]+\\),a=\\([0-9]+\\),s=\\([0-9]+\\)}"
  in
  if string_match regex str 0 then
    let x = int_of_string (matched_group 1 str) in
    let m = int_of_string (matched_group 2 str) in
    let a = int_of_string (matched_group 3 str) in
    let s = int_of_string (matched_group 4 str) in
    { x; m; a; s }
  else failwith "Invalid part"

let parse_workflow str =
  let regex = regexp "\\([a-zA-Z]+\\){\\(.*\\)}" in
  if string_match regex str 0 then
    let name = matched_group 1 str in
    let ops_str = matched_group 2 str in
    let ops = split (regexp ",") ops_str in
    { name; ops = List.map parse_operation ops }
  else failwith "Invalid workflow"

let parse (input : string) : workflows * part t =
  let parts = split (Str.regexp "\n\n") input in
  let workflows = parts |> hd in
  let workflows =
    workflows |> split (Str.regexp "\n") |> List.map parse_workflow
  in
  let workflows =
    workflows
    |> List.fold_left
      (fun acc w ->
         Hashtbl.add acc w.name w;
         acc)
      (Hashtbl.create 10)
  in
  let parts =
    parts |> tl |> hd |> split (Str.regexp "\n") |> List.map parse_part
  in
  (workflows, parts)

(* ============== LOGIC DAY 1 ============== *)

let test_part (p : part) (c : comparison) : bool =
  let value =
    match c.prop with
    | "x" -> p.x
    | "m" -> p.m
    | "a" -> p.a
    | "s" -> p.s
    | _ -> failwith "Invalid property"
  in
  match c.op with Lt -> value < c.value | Gt -> value > c.value

let sort_part_in_workflow (p : part) (workflow : workflow) : string =
  let rec loop (ops : operation t) : string =
    match ops with
    | [] -> failwith "Invalid workflow"
    | op :: [] -> op.next
    | op :: ops -> if test_part p op.comp then op.next else loop ops
  in

  loop workflow.ops

let sort_part (p : part) (workflows : workflows) : bool =
  let rec loop (w : workflow) : bool =
    let next = sort_part_in_workflow p w in
    match next with
    | "R" -> false
    | "A" -> true
    | next -> loop (Hashtbl.find workflows next)
  in
  let initial = Hashtbl.find workflows "in" in
  loop initial

let calc (parts : part t) : int =
  parts |> List.fold_left (fun acc p -> acc + p.x + p.m + p.a + p.s) 0

let logic1 (input : string) : int =
  let workflows, parts = parse input in
  let parts = List.filter (fun p -> sort_part p workflows) parts in
  calc parts

(* ============== LOGIC DAY 2 ============== *)

type range = { min : int; max : int }
type ranges = { x : range; m : range; a : range; s : range }
type step = { ranges : ranges; path : string t }

let max_v = { min = 1; max = 4000 }
let initial_range = { x = max_v; m = max_v; a = max_v; s = max_v }

(* ------- UTILS ------- *)

let print_steps (valid_ranges : step t) =
  print_endline "";
  valid_ranges
  |> List.iter (fun step ->
      Printf.printf "%s\nx:%s m:%s a:%s s:%s\n\n"
        (String.concat " -> " List.(rev step.path))
        (Printf.sprintf "%d-%d" step.ranges.x.min step.ranges.x.max)
        (Printf.sprintf "%d-%d" step.ranges.m.min step.ranges.m.max)
        (Printf.sprintf "%d-%d" step.ranges.a.min step.ranges.a.max)
        (Printf.sprintf "%d-%d" step.ranges.s.min step.ranges.s.max));
  print_endline ""

(* ------- LOGIC ------- *)

type update = { ok : range; nok : range }

let update (r : range) (comp : comparison) : update =
  match comp.op with
  | Lt ->
    {
      ok = { r with max = min r.max (comp.value - 1) };
      nok = { r with min = max r.min comp.value };
    }
  | Gt ->
    {
      ok = { r with min = max r.min (comp.value + 1) };
      nok = { r with max = min r.max comp.value };
    }

let get_valid_ranges (workflows : workflows) =
  let rec loop (step : step) (acc : step t) : step t =
    let last = step.path |> List.hd in
    match last with
    | "R" -> []
    | "A" -> [ step ]
    | name ->
      let workflow = Hashtbl.find workflows name in
      let ranges = ref step.ranges in
      let next_steps =
        workflow.ops
        |> List.map (fun op ->
            let ok_range =
              match op.comp.prop with
              | "x" ->
                let { ok; nok } = update !ranges.x op.comp in
                ranges := { !ranges with x = nok };
                { !ranges with x = ok }
              | "m" ->
                let { ok; nok } = update !ranges.m op.comp in
                ranges := { !ranges with m = nok };
                { !ranges with m = ok }
              | "a" ->
                let { ok; nok } = update !ranges.a op.comp in
                ranges := { !ranges with a = nok };
                { !ranges with a = ok }
              | "s" ->
                let { ok; nok } = update !ranges.s op.comp in
                ranges := { !ranges with s = nok };
                { !ranges with s = ok }
              | _ -> failwith "Invalid property"
            in

            let step =
              { ranges = ok_range; path = op.next :: step.path }
            in
            loop step acc)
      in
      let next_steps = next_steps |> List.concat in
      next_steps @ acc
  in
  let initial = { ranges = initial_range; path = [ "in" ] } in
  loop initial []

(* there is no possible overlap *)
let count (ranges : ranges t) : int =
  ranges
  |> List.fold_left
    (fun acc { x; m; a; s } ->
       acc
       + (x.max - x.min + 1)
         * (m.max - m.min + 1)
         * (a.max - a.min + 1)
         * (s.max - s.min + 1))
    0

let logic2 (input : string) : int =
  let workflows, _ = parse input in
  let steps = get_valid_ranges workflows in
  let ranges = List.map (fun step -> step.ranges) steps in
  count ranges

(* ============== RUN ============== *)

let run (_path : string) =
  (* let input = Parse.read path |> String.concat "\n" in *)
  let input = test_input in
  let result = logic2 input in
  Printf.printf "%d\n" result

(* (* ============== TESTS DAY 1 ============== *) *)
(**)
(* let%test_unit "logic1" = *)
(*   let expected = 19114 in *)
(*   [%test_eq: int] (logic1 test_input) expected *)
(**)
(* let%test_unit "logic1" = *)
(*   let real_input = Parse.read "../inputs/day_19.txt" |> String.concat "\n" in *)
(*   let expected = 432427 in *)
(*   [%test_eq: int] (logic1 real_input) expected *)
(**)
(* (* ============== TESTS DAY 2 ============== *) *)
(**)
(* let%test_unit "logic2" = *)
(*   let expected = 167409079868000 in *)
(*   [%test_eq: int] (logic2 test_input) expected *)
(**)
(* let%test_unit "logic2" = *)
(*   let real_input = Parse.read "../inputs/day_19.txt" |> String.concat "\n" in *)
(*   let expected = 143760172569135 in *)
(*   [%test_eq: int] (logic2 real_input) expected *)
