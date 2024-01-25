open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

(* ============== TYPES ============== *)

let print (map : map) visited : unit =
  let visited = Hashtbl.fold (fun k v acc -> (k, v) :: acc) visited [] in
  let visited = List.map fst visited in
  let map2 = map_from_path visited 'O' '.' in
  let map = merge_map map map2 in
  print_map map

let get_merged_map (map : map) visited (even : bool) : map =
  let visited = Hashtbl.fold (fun k v acc -> (k, v) :: acc) visited [] in
  let visited =
    visited |> List.filter (fun (_, e) -> e = even) |> List.map fst
  in
  let map2 = map_from_path visited 'O' '.' in
  merge_map map map2

let get_map visited (even : bool) : map =
  let visited = Hashtbl.fold (fun k v acc -> (k, v) :: acc) visited [] in
  let visited =
    visited |> List.filter (fun (_, e) -> e = even) |> List.map fst
  in
  let visited = adjust_path visited in
  let map2 = map_from_path visited 'O' '.' in
  map2

(* ============== INPUT ============== *)

let test_input =
  Parse.get_lines
    "...........\n\
     .....###.#.\n\
     .###.##..#.\n\
     ..#.#...#..\n\
     ....#.#....\n\
     .##..S####.\n\
     .##..#...#.\n\
     .......##..\n\
     .##.#.####.\n\
     .##..##.##.\n\
     ..........."

(* ============== LOGIC DAY 1 ============== *)

let move (map : map) (start : point) (steps : int) (even : bool) (limit : int) :
  map * int =
  let m_y, m_x = get_map_dim map in
  let in_bounds = is_in_bounds map in
  (* let cache : cache = Hashtbl.create (130 * 130) in *)
  let visited : (point, bool) Hashtbl.t = Hashtbl.create (130 * 130) in
  let queue : (point, bool) Hashtbl.t ref = ref (Hashtbl.create 130) in
  Hashtbl.add !queue start true;
  let max_y = ref 0 in
  let min_y = ref 1000 in
  let i = ref 0 in
  let has_print = ref false in

  while !i <= steps do
    let even = !i mod 2 = 0 in
    let crt_queue = !queue |> Hashtbl.to_seq_keys |> Queue.of_seq in
    let next_queue = Hashtbl.create 130 in

    while crt_queue |> Queue.is_empty |> not do
      let crt = Queue.pop crt_queue in
      Hashtbl.replace visited crt even;
      max_y := max !max_y crt.y;
      min_y := min !min_y crt.y;

      let next =
        get_neighbours crt
        |> List.filter (fun p ->
            let is_in_bounds = in_bounds p in
            let char =
              if is_in_bounds then map.(p.y).(p.x)
              else
                let mod_x = p.x mod m_x in
                let mod_y = p.y mod m_y in
                let mod_x = if mod_x < 0 then mod_x + m_x else mod_x in
                let mod_y = if mod_y < 0 then mod_y + m_y else mod_y in
                map.(mod_y).(mod_x)
            in
            char <> '#')
      in

      let next = next |> List.filter (fun p -> Hashtbl.mem visited p |> not) in
      next |> List.iter (fun p -> Hashtbl.replace next_queue p even)
    done;
    let max_y_exp = !max_y - !min_y in
    if max_y_exp >= limit then
      if not !has_print then (
        Printf.printf "i: %d\n" !i;
        has_print := true);
    queue := next_queue;
    (* i := steps + 1; *)
    i := !i + 1
  done;

  Printf.printf "limit: %d\n" limit;
  (get_map visited even, !max_y - !min_y)

let logic1 (input : string t) (steps : int) : int =
  let map = input |> map_from_strings in
  let start = find_char_in_map map 'S' in
  let even = steps mod 2 = 0 in
  let map, _ = move map start steps even 1_000 in
  (* print_map map; *)
  count_char_in_map map 'O'

(* ============== LOGIC DAY 2 ============== *)

let l2 (input : string t) (steps : int) : int =
  let map = input |> map_from_strings in
  let n_rows, _ = get_map_dim map in
  (* let map = multiply_map map in *)
  let start = find_char_in_map map 'S' in
  let even = steps mod 2 = 0 in
  let map, max = move map start steps even (n_rows * 2) in

  (* print_map map; *)
  Printf.printf "max: %d\n" max;
  count_char_in_map map 'O'

let l3 (steps : int) : int =
  let f = steps / 131 in
  let x = 14615 in
  let y = 14517 in
  let c = 3676 in
  let result = c + (x * f) + (y * f * f) in
  result

(* we can guess 3676 + 14615x + 14517x^2 *)

(* ============== RUN ============== *)

let run (_path : string) =
  let input = Parse.read _path in
  (* let input = test_input in *)
  let result = logic1 input 64 in
  Printf.printf "%d\n" result

(* ============== TESTS DAY 1 ============== *)

let real_input = Parse.read "../inputs/day_21.txt"

(* let%test_unit "logic1" = *)
(*   let expected = 16 in *)
(*   [%test_eq: int] (logic1 test_input 6) expected *)
(**)
(* let%test_unit "logic1" = *)
(*   let expected = 3578 in *)
(*   [%test_eq: int] (logic1 real_input 64) expected *)
(**)
(* ============== LOGIC DAY 2 ============== *)

(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 10) 50 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 50) 1594 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 100) 6536 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 200) 26538 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 500) 167004 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 1000) 668697 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 test_input 5000) 16733044 *)

(* let%test_unit "l2" = [%test_eq: int] (l2 real_input 65) 3676 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 real_input (131 + 65)) 32_808 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 real_input ((131 * 2) + 65)) 90_974 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 real_input ((131 * 3) + 65)) 178_174 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 real_input ((131 * 4) + 65)) 294_408 *)
(* let%test_unit "l2" = [%test_eq: int] (l2 real_input ((131 * 5) + 65)) 439_676 *)

(* 26501365 = ((131) * 202300) + 65  *)
(* we can guess solve the polynomial equation 131 x + 202300 + 65  = y *)
(* result 3676 + 14615x + 14517x^2 *)
let%test_unit "l3" = [%test_eq: int] (l3 65) 3676
let%test_unit "l3" = [%test_eq: int] (l3 (131 + 65)) 32_808
let%test_unit "l3" = [%test_eq: int] (l3 ((131 * 2) + 65)) 90_974
let%test_unit "l3" = [%test_eq: int] (l3 ((131 * 3) + 65)) 178_174
let%test_unit "l3" = [%test_eq: int] (l3 ((131 * 4) + 65)) 294_408
let%test_unit "l3" = [%test_eq: int] (l3 ((131 * 5) + 65)) 439_676
let%test_unit "l3" = [%test_eq: int] (l3 26501365) 594115391548176
