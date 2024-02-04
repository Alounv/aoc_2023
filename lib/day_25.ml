open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* ============== TYPES ============== *)

type node = { name : string; linked : string list }
type conn = string * string

(* ============== UTILITIES ============== *)

let print_conns (c : conn list) =
  let print_conn (c : conn) = Printf.printf "%s - %s\n" (fst c) (snd c) in
  iter print_conn c;
  Printf.printf "\n"

(* ============== INPUT ============== *)

let test_input =
  Parse.get_lines
    "jqt: rhn xhk nvd\n\
     rsh: frs pzl lsr\n\
     xhk: hfx\n\
     cmg: qnr nvd lhk bvb\n\
     rhn: xhk bvb hfx\n\
     bvb: xhk hfx\n\
     pzl: lsr hfx nvd\n\
     qnr: nvd\n\
     ntq: jqt hfx bvb xhk\n\
     nvd: lhk\n\
     lsr: lhk\n\
     rzs: qnr cmg lsr rsh\n\
     frs: qnr lhk lsr"

(*  ============== PARSING ============== *)

let parse_line (line : string) : node =
  match String.split_on_char ':' line with
  | [ name; linked ] ->
    { name; linked = linked |> String.trim |> String.split_on_char ' ' }
  | _ -> failwith "Invalid input"

let parse (input : string list) =
  input |> map parse_line
  |> map (fun node -> node.linked |> map (fun x -> (node.name, x)))
  |> flatten
  |> map (fun (a, b) -> if a < b then (a, b) else (b, a))

let get_nodes (c : conn list) =
  let nodes = Hashtbl.create 1000 in
  iter
    (fun (a, b) ->
       Hashtbl.replace nodes a
         (b :: (Hashtbl.find_opt nodes a |> Option.value ~default:[]));
       Hashtbl.replace nodes b
         (a :: (Hashtbl.find_opt nodes b |> Option.value ~default:[])))
    c;

  nodes

(* ============== LOGIC ============== *)

(*
 * find shortest path between two nodes 
 *)

let find_shortest_path (connections : conn list) (o : string) (d : string) =
  let nodes = get_nodes connections in
  let visited = Hashtbl.create 1000 in
  let queue = Queue.create () in
  Queue.push [ o ] queue;
  let result = ref [] in

  while not (Queue.is_empty queue) do
    let path = Queue.pop queue in
    let crt = hd path in
    Hashtbl.replace visited crt true;

    if crt = d then (
      result := path;
      Queue.clear queue)
    else
      Hashtbl.find nodes crt
      |> filter (fun x -> not (Hashtbl.mem visited x))
      |> iter (fun x -> Queue.push (x :: path) queue)
  done;
  !result

(*
 * increment the frequency of the edges_frequency
 *)
let find_edges_freq (connections : conn list) edges_frequency (o : string)
    (d : string) =
  let path = find_shortest_path connections o d in

  for i = 0 to length path - 2 do
    let a = nth path i in
    let b = nth path (i + 1) in
    let edge = if a < b then (a, b) else (b, a) in
    let v =
      (Hashtbl.find_opt edges_frequency edge |> Option.value ~default:0) + 1
    in
    Hashtbl.replace edges_frequency edge v
  done;

  ()

(*
 * flood and count the number of nodes in the group
 *)
let flood (connections : conn list) (o : string) : int =
  let nodes = get_nodes connections in
  let visited = Hashtbl.create 1000 in
  let queue = ref [ [ o ] ] in

  while length !queue > 0 do
    match !queue with
    | [] -> ()
    | path :: rest ->
      queue := rest;
      let crt = hd path in
      Hashtbl.replace visited crt true;
      let next = Hashtbl.find_opt nodes crt |> Option.value ~default:[] in
      let next = next |> filter (fun x -> not (Hashtbl.mem visited x)) in
      next |> iter (fun n -> queue := (n :: path) :: !queue)
  done;

  Hashtbl.length visited

(*
 * pick random pairs of nodes 
 *)
let pick_random_pairs (names : string t) (max : int) : (string * string) list =
  let len = length names in
  let res = ref [] in
  for _ = 0 to max do
    let i = Random.int len in
    let j = Random.int len in
    let a = nth names i in
    let b = nth names j in
    res := (a, b) :: !res
  done;
  !res

let rec slice k xs =
  match xs with
  | [] -> failwith "firstk"
  | x :: xs -> if k = 1 then [ x ] else x :: slice (k - 1) xs

let pairs_count = 150

let logic (input : string t) : int =
  let connections = parse input in
  let nodes = get_nodes connections in
  let names = Hashtbl.fold (fun a _ b -> a :: b) nodes [] in
  let len = length names in

  (* find the frequency of the edges with subgraph of 150 nodes *)
  let edges_frequency = Hashtbl.create 1000 in
  let find = find_edges_freq connections edges_frequency in
  pick_random_pairs names pairs_count |> iter (fun (a, b) -> find a b);

  (* find the 3 most frequent edges *)
  let bridges =
    edges_frequency |> Hashtbl.to_seq |> of_seq
    |> sort (fun a b -> snd b - snd a)
    |> map fst |> slice 3
  in
  bridges |> print_conns;

  (* find the number of nodes in the two groups *)
  let remaining = connections |> filter (fun x -> not (mem x bridges)) in
  let g1len = flood remaining "qnr" in
  let g2len = len - g1len in
  g1len * g2len

(* ============== RUN ============== *)

let run (_path : string) =
  let input = Parse.read _path in
  (* let input = test_input in *)
  let result = logic input in
  Printf.printf "---> %d\n" result

(* ============== TESTS ============== *)

(* [ ("hfx", "pzl"); ("bvb", "cmg"); ("nvd", "jqt") ] in *)
let%test_unit "logic" = [%test_eq: int] (logic test_input) 54

(* [ ("lsk", "rfq"); ("qdv", "zhg"); ("gpz", "prk") ] in *)
let%test_unit "logic" =
  let real_input = Parse.read "../inputs/day_25.txt" in
  [%test_eq: int] (logic real_input) 555702
