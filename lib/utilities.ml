open List
(* open ANSITerminal *)

(* data *)

type point = { x : int; y : int }
type path = point t
type path_int = (point * int) t
type line = char array
type map = line array

(* get dimensions of path area *)

let get_path_max (path : path) : point =
  path
  |> List.fold_left
    (fun acc p -> { x = max acc.x p.x; y = max acc.y p.y })
    { x = 0; y = 0 }

let get_path_min (path : path) : point =
  path
  |> List.fold_left
    (fun acc p -> { x = min acc.x p.x; y = min acc.y p.y })
    { x = 0; y = 0 }

let get_map_max (map : map) : point =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in
  { x = max_x - 1; y = max_y - 1 }

let get_map_dim (map : map) : int * int =
  let n_cols = Array.length map.(0) in
  let n_rows = Array.length map in
  (n_rows, n_cols)

let get_column (map : map) (x : int) : char list =
  let max_y = Array.length map in
  let rec aux (y : int) (acc : char list) : char list =
    if y >= max_y then acc else aux (y + 1) (map.(y).(x) :: acc)
  in
  aux 0 []

(* get map from string list *)

let map_from_strings (strings : string list) : map =
  let max_x =
    List.fold_left (fun acc s -> max acc (String.length s)) 0 strings
  in
  let max_y = List.length strings in
  let map = Array.make_matrix max_y max_x ' ' in
  strings
  |> List.iteri (fun y s -> s |> String.iteri (fun x c -> map.(y).(x) <- c));
  map

(* get path from map *)

let get_coords (map : map) (char : char) : point list =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in
  let rec aux (x : int) (y : int) (acc : point list) : point list =
    if y >= max_y then acc
    else if x >= max_x then aux 0 (y + 1) acc
    else
      let c = map.(y).(x) in
      let acc = if c <> char then acc else { x; y } :: acc in
      aux (x + 1) y acc
  in
  aux 0 0 []

(* get map from path *)

let map_from_path (path : path) (path_char : char) (empty_char : char) : map =
  let { x = max_x; y = max_y } = get_path_max path in
  let map = Array.make_matrix (max_y + 1) (max_x + 1) empty_char in
  path |> List.iter (fun p -> map.(p.y).(p.x) <- path_char);
  map

(* get map from path and map *)

let map_from_path_and_map (path : path) (initial_map : map) (empty_char : char)
  : map =
  let { x = max_x; y = max_y } = get_path_max path in
  let map = Array.make_matrix (max_y + 1) (max_x + 1) empty_char in
  path |> List.iter (fun p -> map.(p.y).(p.x) <- initial_map.(p.y).(p.x));
  map

(* print map  *)

type style = { chars : char list; color : int }

let print_map (map : map) : unit =
  print_endline "";
  map
  |> Array.iter (fun x ->
      print_endline
        (x
         |> Array.map (fun x -> Printf.sprintf "%c" x)
         |> Array.to_list |> String.concat ""));
  print_endline ""

let print_map_with_colors (styles : style t) (map : map) : unit =
  print_endline "";
  map
  |> Array.iter (fun x ->
      print_endline
        (x
         |> Array.map (fun x ->
             match find_opt (fun s -> mem x s.chars) styles with
             | Some { color; _ } ->
               Printf.sprintf "\027[38;5;%dm%c\027[0m" color x
             | None -> Printf.sprintf "%c" x)
         |> Array.to_list |> String.concat ""));
  print_endline ""

let string_from_map (map : map) : string =
  map
  |> Array.map (fun x ->
      x
      |> Array.map (fun x -> Printf.sprintf "%c" x)
      |> Array.to_list |> String.concat "")
  |> Array.to_list |> String.concat "\n"

(* print path  *)

let draw_path (path : path) : unit =
  let { x = max_x; y = max_y } = get_path_max path in
  let map = Array.make_matrix (max_y + 1) (max_x + 1) ' ' in
  path
  |> List.iteri (fun i p ->
      if i = 0 then map.(p.y).(p.x) <- 'S'
      else
        let prev = List.nth_opt path (i - 1) in
        let dir =
          match prev with
          | None -> ' '
          | Some prev -> (
              match (p.x - prev.x, p.y - prev.y) with
              | 0, 1 -> 'v'
              | 0, -1 -> '^'
              | 1, 0 -> '>'
              | -1, 0 -> '<'
              | _ -> ' ')
        in
        map.(p.y).(p.x) <- dir);
  print_map map

let print_path (path : path) : unit =
  path |> List.iter (fun p -> Printf.printf "%d-%d\n" p.x p.y)

let print_path_int (path : path_int) : unit =
  path |> List.iter (fun (p, i) -> Printf.printf "%d-%d: %d\n" p.x p.y i)

(* add outside line of empty_char *)

let add_outside_points (map : map) (empty_char : char) : map =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in

  let new_map = Array.make_matrix (max_y + 2) (max_x + 2) empty_char in

  for y = 0 to max_y - 1 do
    for x = 0 to max_x - 1 do
      new_map.(y + 1).(x + 1) <- map.(y).(x)
    done
  done;
  new_map

(* get map from file *)

(* flood fill *)

let flood (map : map) (flood_char : char) empty_char : map =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in

  let rec aux (x : int) (y : int) =
    if x < 0 || x >= max_x || y < 0 || y >= max_y then ()
    else
      let char = map.(y).(x) in
      if char <> empty_char then ()
      else (
        map.(y).(x) <- flood_char;
        aux (x + 1) y;
        aux (x - 1) y;
        aux x (y + 1);
        aux x (y - 1))
  in
  aux 0 0;
  map

(* count chars *)

let count_char_in_map (map : map) (char : char) : int =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in

  let rec aux (x : int) (y : int) (count : int) : int =
    if y >= max_y then count
    else if x >= max_x then aux 0 (y + 1) count
    else
      let c = map.(y).(x) in
      let count = if c <> char then count else count + 1 in
      aux (x + 1) y count
  in
  aux 0 0 0

let count_chars_in_map (map : map) (chars : char list) : int =
  let max_x = Array.length map.(0) in
  let max_y = Array.length map in

  let rec aux (x : int) (y : int) (count : int) : int =
    if y >= max_y then count
    else if x >= max_x then aux 0 (y + 1) count
    else
      let c = map.(y).(x) in
      let count = if mem c chars then count + 1 else count in
      aux (x + 1) y count
  in
  aux 0 0 0

(* transpose map *)

let transpose_map (map : map) : map =
  let n_rows, n_cols = get_map_dim map in
  let new_map = Array.make_matrix n_cols n_rows ' ' in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_map.(j).(i) <- map.(i).(j)
    done
  done;
  new_map

(* rotate map *)

let rotate_map_90 (map : map) : map =
  let n_rows, n_cols = get_map_dim map in
  let new_map = Array.make_matrix n_rows n_cols ' ' in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_map.(i).(j) <- map.(n_rows - j - 1).(i)
    done
  done;
  new_map

let rotate_map_180 (map : map) : map =
  let n_rows, n_cols = get_map_dim map in
  let new_map = Array.make_matrix n_rows n_cols ' ' in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_map.(i).(j) <- map.(n_rows - i - 1).(n_cols - j - 1)
    done
  done;
  new_map

let rotate_map_270 (map : map) : map =
  let n_rows, n_cols = get_map_dim map in
  let new_map = Array.make_matrix n_rows n_cols ' ' in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_map.(i).(j) <- map.(j).(n_cols - i - 1)
    done
  done;
  new_map

(* clone map *)

let clone_map (map : map) : map =
  let n_rows, n_cols = get_map_dim map in
  let new_map = Array.make_matrix n_rows n_cols ' ' in
  for i = 0 to n_rows - 1 do
    for j = 0 to n_cols - 1 do
      new_map.(i).(j) <- map.(i).(j)
    done
  done;
  new_map

(* is out of bounds *)

let is_out_of_bounds (map : map) (p : point) : bool =
  p.x < 0 || p.x >= Array.length map.(0) || p.y < 0 || p.y >= Array.length map

(* neighbors *)

let get_neighbours (pos : point) : point list =
  let { x; y } = pos in
  let up = { x; y = y - 1 } in
  let down = { x; y = y + 1 } in
  let left = { x = x - 1; y } in
  let right = { x = x + 1; y } in
  [ up; down; left; right ]

(* int_of_pos *)
let int_of_pos (map : map) (pos : point) : int =
  let char = map.(pos.y).(pos.x) in
  let value = int_of_char char - 48 in
  value
