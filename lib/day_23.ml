open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open Utilities

(* ============== TYPES ============== *)

(* ============== INPUT ============== *)

let test_input =
  Parse.get_lines
    "\n\
     #.#####################\n\
     #.......#########...###\n\
     #######.#########.#.###\n\
     ###.....#.>.>.###.#.###\n\
     ###v#####.#v#.###.#.###\n\
     ###.>...#.#.#.....#...#\n\
     ###v###.#.#.#########.#\n\
     ###...#.#.#.......#...#\n\
     #####.#.#.#######.#.###\n\
     #.....#.#.#.......#...#\n\
     #.#####.#.#.#########v#\n\
     #.#...#...#...###...>.#\n\
     #.#.#v#######v###.###v#\n\
     #...#.>.#...>.>.#.###.#\n\
     #####v#.#.###v#.#.###.#\n\
     #.....#...#...#.#.#...#\n\
     #.#########.###.#.#.###\n\
     #...###...#...#...#.###\n\
     ###.###.#.###v#####v###\n\
     #...#...#.#.>.>.#.>.###\n\
     #.###.###.#.###.#.#v###\n\
     #.....###...###...#...#\n\
     #####################.#"

let print_situation (map : map) (visited : (string, point * int) Hashtbl.t) =
  let { x = max_x; y = max_y } = get_map_max map in
  let map_2 = Array.make_matrix (max_y + 1) (max_x + 1) ". " in
  visited |> Hashtbl.to_seq_values |> List.of_seq
  |> List.iter (fun (p, v) -> map_2.(p.y).(p.x) <- string_of_int v);

  for y = 0 to max_y do
    for x = 0 to max_x do
      Printf.printf "%s" map_2.(y).(x)
    done;
    print_endline ""
  done;

  ()

(* ============== LOGIC DAY 1 ============== *)

type queue = { p : point; v : int }

let get_next (map : map) (p : point) : point list =
  let { x = max_x; y = max_y } = get_map_max map in
  match map.(p.y).(p.x) with
  | '.' ->
    get_neighbours p
    |> List.filter (fun p ->
        p.x >= 0 && p.x <= max_x && p.y >= 0 && p.y <= max_y
        && map.(p.y).(p.x) <> '#')
  | '>' -> [ { p with x = p.x + 1 } ]
  | '<' -> [ { p with x = p.x - 1 } ]
  | '^' -> [ { p with y = p.y - 1 } ]
  | 'v' -> [ { p with y = p.y + 1 } ]
  | _ -> failwith "invalid"

let move_path (map : map) (start : point) (prev : point) : int * point =
  let rec move (pos : point) (prev : point) (acc : int) : int * point =
    let next = get_next map pos |> List.filter (fun p -> p <> prev) in
    match next with [ next ] -> move next pos (acc + 1) | _ -> (acc, pos)
  in
  move start prev 0

let point_to_string (p : point) : string = Printf.sprintf "%d,%d" p.x p.y

type point_int = { v : int; p : point }
type path_int = point_int list

let get_key (p : point) (prev : point) : string =
  Printf.sprintf "%d,%d,%d,%d" p.x p.y prev.x prev.y

let move (map : map) : int =
  let { x = max_x; y = max_y } = get_map_max map in
  let start = { x = 1; y = 0 } in
  let start = { v = 0; p = start } in
  let final = { x = max_x - 1; y = max_y } in
  let cache = Hashtbl.create 1_000 in
  let i = ref 0 in

  let rec aux (map : map) (crt : point_int) (path : path_int) : path_int list =
    let path_t = path |> List.map (fun p -> p.p) in
    incr i;
    (if !i mod 1_000 = 0 then
       let path = path |> List.map (fun p -> p.p) in
       draw_path path);

    if crt.p = final then [ path ]
    else
      let next =
        get_next map crt.p
        |> List.map (fun p ->
            let key = get_key p crt.p in
            let v =
              match Hashtbl.find_opt cache key with
              | Some v -> v
              | None -> move_path map p crt.p
            in
            Hashtbl.add cache key v;
            v)
        |> List.filter (fun (_, p) -> p <> crt.p)
        |> List.map (fun (v, p) -> { v = crt.v + v + 1; p })
        |> List.filter (fun p -> not (List.mem p.p path_t))
      in

      let results =
        next |> List.map (fun p -> aux map p (p :: path)) |> List.flatten
      in
      results
  in

  let paths = aux map start [ start ] in
  let result =
    paths
    |> List.map (fun path -> path |> List.hd |> fun p -> p.v)
    |> List.fold_left max 0
  in
  result

let logic1 (input : string t) : int =
  let map = map_from_strings input in
  move map

(* ============== LOGIC DAY 2 ============== *)

let remove_slopes (map : map) : map =
  let { x = max_x; y = max_y } = get_map_max map in
  for y = 0 to max_y do
    for x = 0 to max_x do
      match map.(y).(x) with
      | '>' -> map.(y).(x) <- '.'
      | '<' -> map.(y).(x) <- '.'
      | '^' -> map.(y).(x) <- '.'
      | 'v' -> map.(y).(x) <- '.'
      | _ -> ()
    done
  done;
  map

let logic2 (input : string t) : int =
  let map = map_from_strings input in
  let map = remove_slopes map in
  move map

(* ============== RUN ============== *)

let run (_path : string) =
  let input = Parse.read _path in
  (* let input = test_input in *)
  let result = logic2 input in
  Printf.printf "%d\n" result

let real_input = Parse.read "../inputs/day_23.txt"

(* ============== TESTS DAY 1 ============== *)

(* let%test_unit "logic1" = [%test_eq: int] (logic1 test_input) 94 *)
(* let%test_unit "logic1" = [%test_eq: int] (logic1 real_input) 2430 *)

(* ============== LOGIC DAY 2 ============== *)

let%test_unit "logic2" = [%test_eq: int] (logic2 test_input) 154
let%test_unit "logic2" = [%test_eq: int] (logic2 real_input) 6534
