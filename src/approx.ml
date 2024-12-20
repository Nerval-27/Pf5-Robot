open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  {x = (Random.float (rect.x_max -. rect.x_min)) +. rect.x_min ;
   y = (Random.float (rect.y_max -. rect.y_min)) +. rect.y_min}


let translate_all t r =
  match t with
  | Translate (v) -> List.map (transform t) (corners r)
  | Rotate (_, _) -> failwith "Une transformation de type Translate est demandée"

let rotate_all t r =
  match t with
  | Rotate (c, alpha) -> List.map (transform t) (corners r)
  | Translate (_) -> failwith "Une transformation de type Rotate est demandée"

let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with
  | Translate (v) ->
      let translated_corners = translate_all t r in
      rectangle_of_list translated_corners
  | Rotate (c, alpha) ->
      let rotated_corners = rotate_all t r in
      rectangle_of_list rotated_corners


let rec list_last_elem lst =
  match lst with
  | [] -> failwith "Liste vide"
  | [x] -> x
  | _ :: t -> list_last_elem t


let rec run_rect (prog : program) (r : rectangle) : rectangle list =
  let program_V2=unfold_repeat prog in
   List.fold_left (fun acc inst ->
                  match inst with
                  |Move t ->  let last_position = list_last_elem acc in
                        let new_position = transform_rect t last_position in
                        acc @ [new_position]
                  |Either (p',p'')  -> let last_position = list_last_elem acc in
                         if Random.bool () then run_rect p' last_position else run_rect p'' last_position
                  |_ ->  acc @ []
            ) [r] program_V2



let inclusion (r : rectangle) (t : rectangle) : bool =
  let corners_list = corners r in
  List.for_all (fun v -> in_rectangle t v) corners_list




let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool = 
  assert (not (is_deterministic prog));  (* S'assurer que le programme n'est pas déterministe *)
  let possible_execution_list = all_choices prog in
    List.for_all (fun sub_prog ->
      let final_rectangle = List.hd (List.rev (run_rect sub_prog r)) in
      inclusion final_rectangle target ) possible_execution_list



let rec run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
   let program_V2=unfold_repeat prog in
   List.fold_left (fun acc inst ->
                  match inst with
                  |Move t ->  let last_position = list_last_elem acc in
                        let new_position = transform t last_position in
                        acc @ [new_position]
                  |Either (p',p'')  -> let last_position = list_last_elem acc in
                         if Random.bool () then run_polymorphe transform p' last_position else run_polymorphe transform p'' last_position
                  |_ ->  acc 
            ) [i] program_V2



let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  let program_expanded = unfold_repeat prog in
  let result_rectangles = List.fold_left (fun acc inst ->
      match inst with
      | Move t ->
          let last_rectangle = list_last_elem acc in
          let new_rectangle = transform_rect t last_rectangle in
          acc @ [new_rectangle]
      | Either (p1, p2) ->
          let last_rectangle = list_last_elem acc in
          let rect1 = over_approximate p1 last_rectangle in
          let rect2 = over_approximate p2 last_rectangle in
        
          let either_rectangle = rectangle_of_list (corners rect1 @ corners rect2) in
          acc @ [either_rectangle]
      | _ -> acc
    ) [r] program_expanded in
  rectangle_of_list (List.concat_map corners result_rectangles)



let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
    let approximated_rect = over_approximate prog r in
    inclusion approximated_rect target
