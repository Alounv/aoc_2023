(* open Ppx_compare_lib.Builtin *)
(* open Sexplib.Std *)
open List
open Utilities

(* data structures *)

type dir = N | S | E | W

type cust_point = {
  pos : point;
  value : int;
  dir : dir;
  align : int;
  (* we put path_int here only for printing purposes *)
  (* path : path_int; *)
}

(* utilities *)

let char_of_dir (d : dir) : char =
  match d with N -> 'N' | S -> 'S' | E -> 'E' | W -> 'W'

let print_situation (map : map)
    (visited_points : (string, point * int) Hashtbl.t) =
  let { x = max_x; y = max_y } = get_map_max map in
  let map_2 = Array.make_matrix (max_y + 1) (max_x + 1) ' ' in
  Printf.printf "visited: %d\n%!" (Hashtbl.length visited_points);
  visited_points |> Hashtbl.to_seq_values |> List.of_seq
  |> List.iter (fun (p, _) -> map_2.(p.y).(p.x) <- 'X');
  print_map map_2;
  ()

let print_int_map (map : map) =
  print_map_with_colors
    [
      { chars = [ '1' ]; color = 4 };
      { chars = [ '2' ]; color = 4 };
      { chars = [ '3' ]; color = 4 };
      { chars = [ '4' ]; color = 6 };
      { chars = [ '5' ]; color = 2 };
      { chars = [ '6' ]; color = 3 };
      { chars = [ '7' ]; color = 9 };
      { chars = [ '8' ]; color = 5 };
      (* { chars = [ '9' ]; color = 1 }; *)
    ]
    map;
  ()

(* parsing *)

(* input *)

let test_input =
  Parse.get_lines
    "2413432311323\n\
     3215453535623\n\
     3255245654254\n\
     3446585845452\n\
     4546657867536\n\
     1438598798454\n\
     4457876987766\n\
     3637877979653\n\
     4654967986887\n\
     4564679986453\n\
     1224686865563\n\
     2546548887735\n\
     4322674655533"

(* logic 1 *)

let get_dir (current : point) (previous : point) : dir =
  match (current.x - previous.x, current.y - previous.y) with
  | 0, 1 -> S
  | 0, -1 -> N
  | 1, 0 -> E
  | -1, 0 -> W
  | _ -> failwith "invalid path"

let get_aligned_count (prev_dir : dir) (dir : dir) (prev : int) : int =
  if prev_dir = dir then prev + 1 else 1

let get_key (p : cust_point) : string =
  let pos = p.pos in
  let dir = char_of_dir p.dir in
  Printf.sprintf "%d-%d--%c--%d" pos.x pos.y dir p.align

let is_valid (map : map) (best_so_far : int) (prev : cust_point)
    (visited_points : (string, point * int) Hashtbl.t) (max_straigth : int)
    (min_straigth : int) (p : cust_point) : bool =
  let { x; y } = p.pos in
  let { x = max_x; y = max_y } = get_map_max map in
  let v = int_of_pos map p.pos in
  if Hashtbl.mem visited_points (get_key p) then false
  else if
    match (prev.dir, p.dir) with
    | N, S | S, N | E, W | W, E -> true
    | _ -> false
  then false
  else
    let distance_to_end = max_x - x + max_y - y in
    if v + prev.value > best_so_far - distance_to_end then false
    (* check we cannot go too far in the same direction *)
    else if p.align > max_straigth then false
    (* check we MUST go at least some steps in the same direction before turning *)
    else if prev.align < min_straigth && prev.dir <> p.dir then false
    (* check we MUST go at least some steps in the same direction before the final point *)
    else if p.align < min_straigth && p.pos = get_map_max map then false
    else true

(* main function *)

let get_best_value (map : map) (max_straigth : int) (min_straigth : int) : int =
  let start = { x = 0; y = 0 } in
  let final = get_map_max map in
  (* we put path_int here only for printing purposes *)
  let better_values : (string, int) Hashtbl.t = Hashtbl.create 40_000 in
  let visited_points = Hashtbl.create 40_000 in
  let naive_result = 1250 in
  let result = ref naive_result in
  let count = ref 0 in
  let { x = max_x; y = max_y } = get_map_max map in

  let queue =
    ref
      [
        { pos = start; value = 0; dir = E; align = 1 };
        { pos = start; value = 0; dir = S; align = 1 };
      ]
  in

  let rec loop (_ : unit) : unit =
    count := !count + 1;

    (* print situation *)
    if !count mod 10_000 = 0 then print_situation map visited_points;

    (* sort queue to start with lowest value *)
    queue := List.sort (fun a b -> a.value - b.value) !queue;

    (* exit if no more items *)
    match !queue with
    | [] -> ()
    | c :: tail ->
      (* remove current from queue *)
      queue := tail;

      (* add current to visited *)
      let key = get_key c in
      Hashtbl.replace visited_points key (c.pos, c.value);

      (* if current value is higher than best result, continue *)
      if c.value >= !result then loop ()
      (* if current point is final, update result *)
      else if c.pos = final then (
        Printf.printf "--- result: %d, iter: %d ---\n%!" c.value !count;
        print_endline "";
        result := min c.value !result)
      else
        (* else, calculate next points *)
        let get_is_valid =
          is_valid map !result c visited_points max_straigth min_straigth
        in

        c.pos |> get_neighbours
        |> List.filter (fun p ->
            not (p.x < 0 || p.y < 0 || p.x > max_x || p.y > max_y))
        |> List.map (fun pos ->
            let value = c.value + int_of_pos map pos in
            let dir = get_dir pos c.pos in
            let align = get_aligned_count c.dir dir c.align in
            { pos; value; dir; align })
        |> List.filter get_is_valid
        |> List.iter (fun p ->
            (* update and get best value for this position *)
            let key = get_key p in
            let cached = Hashtbl.find_opt better_values key in
            let value =
              match cached with
              | None -> p.value
              | Some cached -> min cached p.value
            in
            Hashtbl.replace better_values key value;
            let p = { p with value } in

            (* add to queue *)
            let q =
              !queue
              |> List.filter (fun q ->
                  q.pos <> p.pos || p.align <> q.align || p.dir <> q.dir)
            in
            queue := p :: q);

        loop ()
  in

  loop ();
  !result

let logic1 (lines : string list) : int =
  let map = map_from_strings lines in
  print_endline "input";
  print_int_map map;
  get_best_value map 3 0

(* logic 2 *)

let logic2 (lines : string list) : int =
  let map = map_from_strings lines in
  print_endline "input";
  print_int_map map;
  get_best_value map 10 4

(* main *)

let run (path : string) =
  let input = Parse.read path in
  let result = logic2 input in
  Printf.printf "%d\n" result

(* (* tests - Part 1 *) *)
(**)
(* let%test_unit "get_best_value" = *)
(*   let test_input = Parse.get_lines "24\n32" in *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_best_value map 3 0 in *)
(*   [%test_eq: int] result 5 *)
(**)
(* let%test_unit "get_best_value" = *)
(*   let test_input = Parse.get_lines "0100\n0440\n4440\n4440" in *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_best_value map 3 0 in *)
(*   [%test_eq: int] result 1 *)
(**)
(* let%test_unit "get_best_value" = *)
(*   let test_input = Parse.get_lines "24134323113\n32154535356\n32552456542" in *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_best_value map 3 0 in *)
(*   [%test_eq: int] result 43 *)
(**)
(* let%test_unit "get_best_value" = *)
(*   let test_input = *)
(*     Parse.get_lines *)
(* "241343231132\n\ *)
   (*        321545353562\n\ *)
   (*        325524565425\n\ *)
   (*        344658584545\n\ *)
   (*        454665786753\n\ *)
   (*        143859879845\n\ *)
   (*        445787698776\n\ *)
   (*        363787797965" *)
(*   in *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_best_value map 3 0 in *)
(*   [%test_eq: int] result 71 *)
(**)
(* let%test_unit "logic1" = *)
(*   let expected = 102 in *)
(*   [%test_eq: int] (logic1 test_input) expected *)
(**)
(* (* let%test_unit "logic1" = *) *)
(* (*   let real_input = Parse.read "../inputs/day_17.txt" in *) *)
(* (*   let expected = 1076 in *) *)
(* (*   [%test_eq: int] (logic1 real_input) expected *) *)
(**)
(* (* tests - Part 2 *) *)
(**)
(* let%test_unit "get_best_value" = *)
(*   let test_input = *)
(*     Parse.get_lines *)
(*       "111111111111\n999999999991\n999999999991\n999999999991\n999999999991" *)
(*   in *)
(*   let map = map_from_strings test_input in *)
(*   let result = get_best_value map 10 4 in *)
(*   [%test_eq: int] result 71 *)
(**)
(* let%test_unit "logic2" = *)
(*   let expected = 94 in *)
(*   [%test_eq: int] (logic2 test_input) expected *)
(**)
(* (* let%test_unit "logic1" = *) *)
(* (*   let real_input = Parse.read "../inputs/day_17.txt" in *) *)
(* (*   let expected = 1219 in *) *)
(* (*   [%test_eq: int] (logic2 real_input) expected *) *)
