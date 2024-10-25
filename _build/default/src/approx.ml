open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  {x = (Random.float (rect.x_max -. rect.x_min)) +. rect.x_min ;
   y = (Random.float (rect.y_max -. rect.y_min)) +. rect.y_min}
  
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  failwith "À compléter"

let run_rect (prog : program) (r : rectangle) : rectangle list =
  failwith "À compléter"

let inclusion (r : rectangle) (t : rectangle) : bool =
  let corners_list = corners r in 
  List.for_all (fun v -> in_rectangle t v) corners_list

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  assert (not (is_deterministic prog));
  let possible_execution_list = all_choices prog in
  List.for_all (fun sub_prog -> inclusion target (List.nth (run_rect sub_prog r) ((List.length sub_prog) - 1))) possible_execution_list

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
