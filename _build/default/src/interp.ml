open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list

let rec is_deterministic (prog : program) : bool =
  match prog with
  |[] -> true
  |hd::tl -> match hd with
    |Move _ -> is_deterministic tl
    |Repeat (_,repeat_prog) -> is_deterministic repeat_prog
    |Either _ -> false

let unfold_repeat (prog : program) : program =
  failwith "À compléter"

let run_det (prog : program) (p : point) : point list =
  failwith "À compléter"

let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  assert (is_deterministic prog);
  in_rectangle target (List.hd (List.rev (run_det prog p)))
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  assert (not (is_deterministic prog));
  let possible_execution_list = all_choices prog in
  List.for_all (fun sub_prog -> in_rectangle r (List.hd (List.rev (run sub_prog p)))) possible_execution_list
