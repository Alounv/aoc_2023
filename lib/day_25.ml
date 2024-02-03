open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
(* open Str *)
(* open Utilities *)

(* ============== TYPES ============== *)

type node = { name : string; linked : string list }
type connection = string * string

(* ============== UTILITIES ============== *)

let print_connection (c : connection) =
  Printf.printf "%s - %s\n" (fst c) (snd c)

let print_connections (c : connection list) =
  List.iter print_connection c;
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

let parse_line (line : string) : node =
  match String.split_on_char ':' line with
  | [ name; linked ] ->
    { name; linked = linked |> String.trim |> String.split_on_char ' ' }
  | _ -> failwith "Invalid input"

let parse (input : string list) =
  input |> List.map parse_line
  |> List.map (fun node -> node.linked |> List.map (fun x -> (node.name, x)))
  |> List.flatten
  |> List.map (fun (a, b) -> if a < b then (a, b) else (b, a))

let get_nodes (c : connection list) =
  let nodes = Hashtbl.create 1000 in
  List.iter
    (fun (a, b) ->
       Hashtbl.replace nodes a
         (b :: (Hashtbl.find_opt nodes a |> Option.value ~default:[]));
       Hashtbl.replace nodes b
         (a :: (Hashtbl.find_opt nodes b |> Option.value ~default:[])))
    c;

  nodes

(* ============== LOGIC DAY 1 ============== *)

let find_edges_frequency (connections : connection list) edges_frequency
    (o : string) (d : string) =
  let nodes = get_nodes connections in
  let visited = Hashtbl.create 1000 in
  let queue = ref [ [ o ] ] in
  let shortest_path = ref [] in

  while List.length !queue > 0 do
    queue := List.sort (fun a b -> List.length a - List.length b) !queue;
    match !queue with
    | [] -> ()
    | path :: rest ->
      queue := rest;
      let crt = List.hd path in
      Hashtbl.replace visited crt true;
      if crt = d then (
        shortest_path := path;
        queue := [])
      else
        let next = Hashtbl.find nodes crt in
        let next =
          next |> List.filter (fun x -> not (Hashtbl.mem visited x))
        in
        next
        |> List.iter (fun x ->
            let path = x :: path in
            queue := path :: !queue)
  done;

  let path = !shortest_path in
  for i = 0 to List.length path - 2 do
    let a = List.nth path i in
    let b = List.nth path (i + 1) in
    let edge = if a < b then (a, b) else (b, a) in
    let v =
      (Hashtbl.find_opt edges_frequency edge |> Option.value ~default:0) + 1
    in
    Hashtbl.replace edges_frequency edge v
  done;

  ()

let flood (connections : connection list) (o : string) : int =
  let nodes = get_nodes connections in
  let visited = Hashtbl.create 1000 in
  let queue = ref [ [ o ] ] in

  while List.length !queue > 0 do
    match !queue with
    | [] -> ()
    | path :: rest ->
      queue := rest;
      let crt = List.hd path in
      Hashtbl.replace visited crt true;
      let next = Hashtbl.find_opt nodes crt |> Option.value ~default:[] in
      let next = next |> List.filter (fun x -> not (Hashtbl.mem visited x)) in
      next
      |> List.iter (fun x ->
          let path = x :: path in
          queue := path :: !queue)
  done;

  Hashtbl.length visited

let rec slice k xs =
  match xs with
  | [] -> failwith "firstk"
  | x :: xs -> if k = 1 then [ x ] else x :: slice (k - 1) xs

let logic1 (input : string t) : int =
  let connections = parse input in
  let nodes = get_nodes connections in
  let names = Hashtbl.fold (fun a _ b -> a :: b) nodes [] in
  let len = List.length names in

  let edges_frequency = Hashtbl.create 1000 in
  let find = find_edges_frequency connections edges_frequency in

  for c = 0 to 150 do
    Printf.printf "%d\n%!" c;
    let i = Random.int len in
    let j = Random.int len in
    let a = List.nth names i in
    let b = List.nth names j in
    find a b
  done;
  let res = edges_frequency |> Hashtbl.to_seq |> List.of_seq in
  let res = res |> List.sort (fun a b -> snd b - snd a) |> List.map fst in

  let bridges = slice 3 res in

  bridges |> print_connections;
  let cLeft = connections |> List.filter (fun x -> not (List.mem x bridges)) in
  let start = "qnr" in
  let g1len = flood cLeft start in
  let g2len = len - g1len in
  g1len * g2len

(* ============== RUN ============== *)

let run (_path : string) =
  let input = Parse.read _path in
  (* let input = test_input in *)
  let result = logic1 input in
  Printf.printf "---> %d\n" result

(* ============== TESTS DAY 1 ============== *)

let%test_unit "logic1" = [%test_eq: int] (logic1 test_input) 54

let%test_unit "logic1" =
  let real_input = Parse.read "../inputs/day_25.txt" in
  [%test_eq: int] (logic1 real_input) 555702

(* [ ("hfx", "pzl"); ("bvb", "cmg"); ("nvd", "jqt") ] in *)
(* [ ("lsk", "rfq"); ("qdv", "zhg"); ("gpz", "prk") ] in *)
