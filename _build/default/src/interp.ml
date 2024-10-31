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

let rec unfold_repeat (prog : program) : program =
  let rec prog_reap n repeat_prog =
    match n with
    | 0 -> []
    | n when n > 0 -> repeat_prog @ prog_reap (n - 1) repeat_prog
    | _ -> failwith "Le nombre de répétitions doit être positif"
  in
  match prog with
  | [] -> []
  | hd :: tl ->
      match hd with
      | Move _ -> hd :: unfold_repeat tl
      | Repeat (n, repeat_prog) ->
          prog_reap n repeat_prog @ unfold_repeat tl
      | Either (prog1, prog2) ->
          Either (unfold_repeat prog1, unfold_repeat prog2) :: unfold_repeat tl


let rec list_last_elem lst =
  match lst with
  | [] -> failwith "Liste vide"
  | [x] -> x
  | _ :: t -> list_last_elem t


let rec run_det (prog : program) (p : point) : point list =
  let program_V2 = unfold_repeat prog in
  List.fold_left
    (fun acc inst ->
      match inst with
      | Move t ->
          let last_position = list_last_elem acc in
          let new_position = transform t last_position in
          acc @ [new_position]
      | _ -> failwith "Instruction Either ou Repeat rencontrée dans run_det")
    [p] program_V2



let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  assert (is_deterministic prog);
  in_rectangle target (List.hd (List.rev (run_det prog p)))


let rec run (prog : program) (p : point) : point list =
  let program_V3=unfold_repeat prog in
   List.fold_left (fun acc inst ->
                  match inst with
                  |Move t ->  let last_position = list_last_elem acc in
                        let new_position = transform t last_position in
                        acc @ [new_position]
                  |Either (p',p'')  -> let last_position = list_last_elem acc in
                         if Random.bool () then run p' last_position else run p'' last_position
                  |_ ->  failwith "Instruction Repeat non dépliée"
            ) [p] program_V3


let rec all_choices (prog : program) : program list =
  let rec aux prog =
    match prog with
    | [] -> [[]]
    | Move t :: tl ->
        let rest_choices = aux tl in
        List.map (fun rest -> Move t :: rest) rest_choices
    | Either (p1, p2) :: tl ->
        let choices_p1 = aux p1 in
        let choices_p2 = aux p2 in
        let rest_choices = aux tl in
        List.concat_map (fun choice1 ->
          List.map (fun rest -> choice1 @ rest) rest_choices
        ) (choices_p1 @ choices_p2)
    | Repeat (_, _) :: _ -> failwith "Les Repeat doivent être supprimés"
  in
  aux (unfold_repeat prog)


let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  assert (not (is_deterministic prog));
  let possible_execution_list = all_choices prog in
  List.for_all (fun sub_prog -> in_rectangle r (List.hd (List.rev (run sub_prog p)))) possible_execution_list
