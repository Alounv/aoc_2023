open Ppx_compare_lib.Builtin
open Sexplib.Std
open List
open String

(* open Str *)
open Utilities

(* ============== TYPES ============== *)

type brick = {
  id : string;
  x1 : int;
  y1 : int;
  z1 : int;
  x2 : int;
  y2 : int;
  z2 : int;
  supports : string list;
}

type map = (int * string) array array

(* ============== UTILS ============== *)

let parse_brick (index : int) (str : string) : brick =
  let parts = split_on_char '~' str in
  let c1 = split_on_char ',' (nth parts 0) in
  let c2 = split_on_char ',' (nth parts 1) in
  let p (c, i) = int_of_string (nth c i) in
  let id = int_to_letters (index + 1) in
  {
    id;
    x1 = p (c1, 0);
    y1 = p (c1, 1);
    z1 = p (c1, 2);
    x2 = p (c2, 0);
    y2 = p (c2, 1);
    z2 = p (c2, 2);
    supports = [];
  }

let print_brick (b : brick) : unit =
  let supports = String.concat ", " b.supports in
  Printf.printf "%s: (%d, %d, %d) ~ (%d, %d, %d) [%s]\n" b.id b.x1 b.y1 b.z1
    b.x2 b.y2 b.z2 supports

let print_bricks (bricks : brick list) : unit =
  bricks |> List.iter print_brick;
  print_newline ()

let parse (input : string list) : brick list = input |> List.mapi parse_brick

let get_max (bricks : brick list) : int * int =
  bricks
  |> List.map (fun brick -> (max brick.x1 brick.x2, max brick.y1 brick.y2))
  |> List.fold_left
    (fun (acc_x, acc_y) (x, y) -> (max acc_x x, max acc_y y))
    (0, 0)

(* ============== INPUT ============== *)

let test_input =
  Parse.get_lines
    "1,0,1~1,2,1\n\
     0,0,2~2,0,2\n\
     0,2,3~2,2,3\n\
     0,0,4~0,2,4\n\
     2,0,5~2,2,5\n\
     0,1,6~2,1,6\n\
     1,1,8~1,1,9"

(* ============== LOGIC ============== *)

(* ============== LOGIC DAY 1 ============== *)

let get_highest_ground (ground : map) (b : brick) : int * string list =
  let z = ref 0 in
  for x = b.x1 to b.x2 do
    for y = b.y1 to b.y2 do
      z := max !z (fst ground.(x).(y))
    done
  done;

  let ids = ref [] in
  for x = b.x1 to b.x2 do
    for y = b.y1 to b.y2 do
      let gz, id = ground.(x).(y) in
      if gz = !z then if not (List.mem id !ids) then ids := id :: !ids
    done
  done;
  (!z, !ids)

let move_brick (low_z : int) (b : brick) : brick =
  if b.z1 > b.z2 then { b with z1 = low_z + (b.z1 - b.z2); z2 = low_z }
  else { b with z1 = low_z; z2 = low_z + (b.z2 - b.z1) }

let update_ground (ground : map) (b : brick) : unit =
  for x = b.x1 to b.x2 do
    for y = b.y1 to b.y2 do
      ground.(x).(y) <- (max b.z1 b.z2, b.id)
    done
  done

let move (sorted_bricks : brick list) : brick list =
  let max_x, max_y = get_max sorted_bricks in
  let ground = Array.make_matrix (max_x + 1) (max_y + 1) (0, "_") in
  sorted_bricks
  |> List.map (fun b ->
      let gz, supports = get_highest_ground ground b in
      let low_z = gz + 1 in
      let b = move_brick low_z b in
      update_ground ground b;
      { b with supports })

let get_supporting_bricks (bricks : brick list) : string list =
  bricks
  |> List.filter (fun b -> List.length b.supports = 1 && b.supports <> [ "_" ])
  |> List.map (fun b -> b.supports)
  |> List.flatten |> List.sort_uniq compare

let count (bricks : brick list) : int =
  let risks = bricks |> get_supporting_bricks |> List.length in
  List.length bricks - risks

let logic1 (input : string list) : int =
  let bricks = parse input in
  let bricks = bricks |> List.sort (fun a b -> min a.z1 a.z2 - min b.z1 b.z2) in
  let bricks = move bricks in
  count bricks

(* ============== LOGIC DAY 2 ============== *)

type supported = (string, string list) Hashtbl.t

let get_supported (bricks : brick list) : supported =
  let supported : supported = Hashtbl.create 1_000 in
  bricks |> List.iter (fun b -> Hashtbl.add supported b.id [ b.id ]);
  for i = 0 to List.length bricks - 1 do
    let b = nth bricks i in
    let value = Hashtbl.find supported b.id in

    b.supports
    |> List.filter (fun s -> s <> "_" && s <> b.id)
    |> List.iter (fun s ->
        let c = Hashtbl.find supported s in
        let v = value @ c |> List.sort_uniq compare in
        Hashtbl.replace supported s v)
  done;
  supported

let count_fallen (supp : supported) (bricks : brick list) (origin : string) :
  int =
  let fallen = Hashtbl.create 1_000 in
  let count = ref 0 in

  let rec loop (id : string) (acc : int) : unit =
    count := !count + 1;
    Printf.printf "%d\n" !count;
    Hashtbl.replace fallen id true;

    Hashtbl.find supp id
    |> List.filter (fun s_id ->
        if s_id = "_" || s_id = id then false
        else
          let brick = List.find (fun b -> b.id = s_id) bricks in
          List.for_all (fun s -> Hashtbl.mem fallen s) brick.supports)
    |> List.iter (fun id -> loop id (acc + 1))
  in
  loop origin 0;
  print_newline ();
  Hashtbl.length fallen - 1

let count2 (bricks : brick list) : int =
  let supported = get_supported bricks in
  let supporting = bricks |> get_supporting_bricks in
  let get_brick_count = count_fallen supported bricks in
  supporting |> List.fold_left (fun acc b -> acc + get_brick_count b) 0

let logic2 (input : string list) : int =
  let bricks = parse input in
  let bricks = bricks |> List.sort (fun a b -> min a.z1 a.z2 - min b.z1 b.z2) in
  let bricks = move bricks in
  let bricks = bricks |> List.sort (fun a b -> min b.z1 b.z2 - min a.z1 a.z2) in
  print_bricks bricks;
  count2 bricks

(* ============== RUN ============== *)

let run (path : string) =
  let input = Parse.read path in
  (* let input = test_input in *)
  let result = logic2 input in
  Printf.printf "%d\n" result

let real_input = Parse.read "../inputs/day_22.txt"

(* ============== TESTS DAY 1 ============== *)

let%test_unit "logic1" = [%test_eq: int] (logic1 test_input) 5
let%test_unit "logic1" = [%test_eq: int] (logic1 real_input) 515

(* ============== LOGIC DAY 2 ============== *)

let%test_unit "logic2" = [%test_eq: int] (logic2 test_input) 7
let%test_unit "logic2" = [%test_eq: int] (logic2 real_input) 101_541
