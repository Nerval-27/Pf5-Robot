open Pf5;;
open Geo;;
type opt = {
  abs: Pf5.Geo.rectangle option * bool;
  cr: bool;
  bc: (int * int * int);
  fc: (int * int * int);
  rc: (int * int * int);
  pc: (int * int * int);
  size: (int * int);
  prog: int;
  print: bool; 
}

let convert_of_float x = try
  Some (float_of_string x)
with
| Failure _ -> None   

let convert_of_int x = try
  Some (int_of_string x)
with
| Failure _ -> None   


let convert_to_float x =
  try Some (float_of_string x) with
  | Failure _ -> None

let convert_to_int x =
  try Some (int_of_string x) with
  | Failure _ -> None


let rec parse argv index options_update =
  if index >= Array.length argv then
    options_update
  else
    match argv.(index) with
    | "-abs" ->
      let x_min = convert_to_float argv.(index + 1) in
      let y_min= convert_to_float argv.(index + 2) in
      let x_max = convert_to_float argv.(index + 3) in
      let y_max = convert_to_float argv.(index + 4) in
      (match x_min, y_min, x_max, y_max with
      | Some a, Some b, Some c, Some d ->
        let rectangle = { x_min=a; y_min=b ;x_max=c ;y_max=d } in
        parse argv (index + 5) { options_update with abs = (Some rectangle, true) }
      | _ -> failwith "Invalid parameters for -abs")

    | "-cr" ->
      parse argv (index + 1) { options_update with cr = true }

    | "-bc" ->
      let r = convert_to_int argv.(index + 1) in
      let g = convert_to_int argv.(index + 2) in
      let b = convert_to_int argv.(index + 3) in
      (match r, g, b with
        | Some r', Some g', Some b' -> parse argv (index + 4) { options_update with bc = (r', g', b') }
        | _ -> failwith "Invalid parameters for -bc")

    | "-fc" ->
      let r = convert_to_int argv.(index + 1) in
      let g = convert_to_int argv.(index + 2) in
      let b = convert_to_int argv.(index + 3) in
      (match r, g, b with
      | Some r', Some g', Some b' -> parse argv (index + 4) { options_update with fc = (r', g', b') }
      | _ -> failwith "Invalid parameters for -fc")

    | "-rc" ->
      let r = convert_to_int argv.(index + 1) in
      let g = convert_to_int argv.(index + 2) in
      let b = convert_to_int argv.(index + 3) in
      (match r, g, b with
      | Some r', Some g', Some b' -> parse argv (index + 4) { options_update with rc = (r', g', b') }
      | _ -> failwith "Invalid parameters for -rc")

    | "-pc" ->
      let r = convert_to_int argv.(index + 1) in
      let g = convert_to_int argv.(index + 2) in
      let b = convert_to_int argv.(index + 3) in
      (match r, g, b with
      | Some r', Some g', Some b' -> parse argv (index + 4) { options_update with pc = (r', g', b') }
      | _ -> failwith "Invalid parameters for -pc")

    | "-size" ->
      let w = convert_to_int argv.(index + 1) in
      let h = convert_to_int argv.(index + 2) in
      (match w, h with
      | Some w', Some h' -> parse argv (index + 3) { options_update with size = (w', h') }
      | _ -> failwith "Invalid parameters for -size")
    
    | "-print" ->
      parse argv (index + 1) { options_update with print = true }
  

    | "1" | "2" | "3" ->
      (match convert_to_int argv.(index) with
      | Some prog -> parse argv (index + 1) { options_update with prog }
      | None -> failwith "Invalid program number")

   
    | _ ->
      parse argv (index + 1) options_update



   