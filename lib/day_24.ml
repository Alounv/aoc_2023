open Ppx_compare_lib.Builtin
open Sexplib.Std
open List

(* open Str *)
open Utilities

(* ============== TYPES ============== *)

type particle = {
  id : string;
  pos : float * float * float;
  vel : float * float * float;
}

type intersection = { x : float; y : float; t : float }

(* ============== PRINT ============== *)

let intersection_to_string (c : intersection) : string =
  Printf.sprintf "(%f, %f) @ %f" c.x c.y c.t

(* ============== INPUT ============== *)

let real_input = Parse.read "../inputs/day_24.txt"

let test_input =
  Parse.get_lines
    "19, 13, 30 @ -2,  1, -2\n\
     18, 19, 22 @ -1, -1, -2\n\
     20, 25, 34 @ -2, -2, -4\n\
     12, 31, 28 @ -1, -2, -1\n\
     20, 19, 15 @  1, -5, -3"

let parse_line (i : int) (line : string) : particle =
  let parts = Str.split (Str.regexp "[^0-9-]+") line in
  let pos =
    ( nth parts 0 |> int_of_string |> float_of_int,
      nth parts 1 |> int_of_string |> float_of_int,
      nth parts 2 |> int_of_string |> float_of_int )
  in
  let vel =
    ( nth parts 3 |> int_of_string |> float_of_int,
      nth parts 4 |> int_of_string |> float_of_int,
      nth parts 5 |> int_of_string |> float_of_int )
  in
  { id = int_to_letters (i + 1); pos; vel }

let parse (input : string list) : particle list = mapi parse_line input

(* ============== LOGIC ============== *)

let get_intersection (p1 : particle) (p2 : particle) : intersection option =
  if p1.id = p2.id then None
  else
    let x1, y1, _ = p1.pos in
    let vx1, vy1, _ = p1.vel in
    let x2, y2, _ = p2.pos in
    let vx2, vy2, _ = p2.vel in
    let a =
      (((x2 -. x1) *. vy2) -. ((y2 -. y1) *. vx2))
      /. ((vx1 *. vy2) -. (vy1 *. vx2))
    in
    let b =
      (((x1 -. x2) *. vy1) -. ((y1 -. y2) *. vx1))
      /. ((vx2 *. vy1) -. (vy2 *. vx1))
    in
    if a < 0. || b < 0. then None
    else
      let x = x1 +. (a *. vx1) in
      let y = y1 +. (a *. vy1) in
      Some { x; y; t = a }

let is_valid (max : float) (min : float) (i : intersection) : bool =
  i.t >= 0. && i.x >= min && i.x <= max && i.y >= min && i.y <= max

let map_valid (max : float) (min : float) (c : intersection option) :
  intersection option =
  match c with
  | None -> None
  | Some c -> if is_valid max min c then Some c else None

(* ============== LOGIC DAY 1 ============== *)

let combine (particles : particle list) (max : float) (min : float) :
  intersection list =
  let rec aux (particles : particle list) (acc : intersection list) :
    intersection list =
    match particles with
    | [] -> acc
    | p :: ps ->
      let intersections = ps |> map (get_intersection p) in
      let map_valid = map_valid max min in
      let valid = intersections |> filter_map map_valid in
      aux ps (acc @ valid)
  in
  aux particles []

let logic1 (input : string t) (max : float) (min : float) : int =
  let particles = parse input in
  let intersections = combine particles max min in
  List.length intersections

(* ============== LOGIC DAY 2 ============== *)

(* type collision = { x : float; y : float; z : float; t : float } *)
(**)
(* let get_collision (p1 : particle) (p2 : particle) : collision option = *)
(*   if p1.id = p2.id then None *)
(*   else *)
(*     let x1, y1, z1 = p1.pos in *)
(*     let vx1, vy1, vz1 = p1.vel in *)
(*     let x2, y2, z2 = p2.pos in *)
(*     let vx2, vy2, vz2 = p2.vel in *)
(**)
(*     if vx1 = vx2 && vy1 = vy2 then None *)
(*     else if vy1 <> vy2 then *)
(*       let t = (y1 -. y2) /. (vy2 -. vy1) in *)
(*       let xt1 = x1 +. (t *. vx1) in *)
(*       let xt2 = x2 +. (t *. vx2) in *)
(*       if xt1 <> xt2 then None *)
(*       else *)
(*         let yt = y1 +. (t *. vy1) in *)
(*         let zt1 = z1 +. (t *. vz1) in *)
(*         let zt2 = z2 +. (t *. vz2) in *)
(*         if zt1 <> zt2 then Some { x = xt1; y = yt; z = 0.; t } *)
(*         else Some { x = xt1; y = yt; z = zt1; t } *)
(*     else *)
(*       let t = (x1 -. x2) /. (vx2 -. vx1) in *)
(*       let yt1 = y1 +. (t *. vy1) in *)
(*       let yt2 = y2 +. (t *. vy2) in *)
(*       if yt1 <> yt2 then None *)
(*       else *)
(*         let xt = x1 +. (t *. vx1) in *)
(*         let zt1 = z1 +. (t *. vz1) in *)
(*         let zt2 = z2 +. (t *. vz2) in *)
(*         if zt1 <> zt2 then Some { x = xt; y = yt1; z = 0.; t } *)
(*         else Some { x = xt; y = yt1; z = zt1; t } *)
(**)
(* let is_valid (max : float) (min : float) (c : collision) : bool = *)
(*   c.t > 0. && c.x >= min && c.x <= max && c.y >= min && c.y <= max *)
(**)
(* let map_valid (max : float) (min : float) (c : collision option) : *)
(*   collision option = *)
(*   match c with *)
(*   | None -> None *)
(*   | Some c -> if is_valid max min c then Some c else None *)

(**)
(* ============== RUN ============== *)

let min = 200000000000000.
let max = 400000000000000.

let run (_path : string) =
  (* let input = Parse.read _path in *)
  let input = test_input in
  let result = logic1 input max min in
  Printf.printf "%d\n" result

(* ============== TESTS DAY 1 ============== *)

let%test_unit "logic1" = [%test_eq: int] (logic1 test_input 27. 7.) 2
let%test_unit "logic1" = [%test_eq: int] (logic1 real_input max min) 20847

(* ============== LOGIC DAY 2 ============== *)
