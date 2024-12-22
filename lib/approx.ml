open Geo
open Interp

(* Code de la Section 5 du projet. *)

(* Fonction qui génère un point aléatoire à l'intérieur d'un rectangle donné `rect`. *)
let sample (rect : rectangle) : point =
  {x = (Random.float (rect.x_max -. rect.x_min)) +. rect.x_min ;  (* coordonnée x aléatoire dans le rectangle. *)
   y = (Random.float (rect.y_max -. rect.y_min)) +. rect.y_min}   (* coordonnée y aléatoire dans le rectangle. *)

(* Fonction qui applique une transformation de type `Translate` à tous les coins d'un rectangle `r`. *)
let translate_all t r =
  match t with
  | Translate (v) -> List.map (transform t) (corners r)  (* Applique la transformation à tous les coins du rectangle. *)
  | Rotate (_, _) -> failwith "Une transformation de type Translate est demandée"  (* Si la transformation est de type `Rotate`, une exception est levée. *)

(* Fonction qui applique une transformation de type `Rotate` à tous les coins d'un rectangle `r`. *)
let rotate_all t r =
  match t with
  | Rotate (c, alpha) -> List.map (transform t) (corners r)  (* Applique la transformation `Rotate` à tous les coins du rectangle. *)
  | Translate (_) -> failwith "Une transformation de type Rotate est demandée"  (* Si la transformation est de type `Translate`, une erreur est levée. *)

(* Fonction qui applique une transformation donnée `t` à un rectangle `r`. *)
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with
  | Translate (v) ->  (* translation = on applique `translate_all` à tous les coins. *)
      let translated_corners = translate_all t r in
      rectangle_of_list translated_corners  (* Convertit la liste des coins transformés en un rectangle. *)

  | Rotate (c, alpha) ->  (* rotation =  on applique `rotate_all` à tous les coins. *)
      let rotated_corners = rotate_all t r in
      rectangle_of_list rotated_corners  (* Convertit la liste des coins transformés en un rectangle. *)

(* Fonction `list_last_elem` qui renvoie le dernier élément d'une liste et lève une exception si la liste est vide. *)
let rec list_last_elem lst =
  match lst with
  | [] -> failwith "Liste vide"  (* Si la liste est vide, on lève une exception. *)
  | [x] -> x  (* Si c'est le dernier élément, on le renvoie. *)
  | _ :: t -> list_last_elem t  (* On continue à parcourir la liste pour trouver le dernier élément. *)

(* Fonction qui exécute un programme sur un rectangle donné `r`. 
   Elle génère une liste des rectangles successifs générés par l'exécution des instructions. *)
   let rec run_rect (prog : program) (r : rectangle) : rectangle list =
    Random.self_init ();
    (* Déplier les instructions Repeat dans le programme pour simplifier le traitement *)
    let program_V2 = unfold_repeat prog in
  
    (* Appliquer les instructions au rectangle de départ *)
    List.fold_left (fun acc inst ->
      match inst with
  
      (* Cas Move : appliquer une transformation au dernier rectangle de la liste *)
      | Move t -> 
          let last_rectangle = list_last_elem acc in
          let new_rectangle = transform_rect t last_rectangle in
          acc @ [new_rectangle]
  
      (* Cas Either : choisir aléatoirement une branche et l'exécuter *)
      | Either (p1, p2) -> 
          let last_rectangle = list_last_elem acc in
          
          (* Faire un choix aléatoire entre les deux branches *)
          let chosen_prog = if Random.bool () then p1 else p2 in
          let branch_result = run_rect chosen_prog last_rectangle in
          
          (* Ajouter les rectangles générés par la branche choisie *)
          acc @ branch_result
  
      (* Cas Repeat ne devrait pas exister après unfold_repeat *)
      | Repeat _ -> failwith "Les instructions Repeat doivent être supprimées par unfold_repeat"
  
    ) [r] program_V2  (* Initialiser avec le rectangle de départ `r`. *)
  
(* Fonction qui vérifie si tous les coins du rectangle `r` sont à l'intérieur du rectangle `t`. *)
let inclusion (r : rectangle) (t : rectangle) : bool =
  let corners_list = corners r in
  List.for_all (fun v -> in_rectangle t v) corners_list

(* Fonction qui vérifie si un programme atteint un rectangle cible `target` à partir d'un rectangle `r`. 
   Elle exécute toutes les variantes possibles du programme et vérifie si le rectangle final se trouve dans la cible. *)
let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool = 
  let possible_execution_list = all_choices prog in
  List.for_all (fun sub_prog ->
    let final_rectangle = List.hd (List.rev (run_rect sub_prog r)) in
    inclusion final_rectangle target ) possible_execution_list

(* Fonction qui exécute un programme avec une transformation polymorphe `transform`
   et renvoie une liste des éléments successifs générés par l'exécution des instructions. *)
let rec run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  let program_V2 = unfold_repeat prog in  (* On déplie les répétitions dans le programme. *)
  List.fold_left (fun acc inst ->  (* On parcourt chaque instruction du programme. *)
    match inst with
    | Move t ->  (* on applique la transformation à l'élément courant. *)
        let last_position = list_last_elem acc in  (* on récupère la dernière position de la liste. *)
        let new_position = transform t last_position in  (* on applique la transformation sur cette position. *)
        acc @ [new_position]  (* On ajoute le nouvel élément transformé à la liste des résultats. *)

    | Either (p', p'') ->  (* on choisit aléatoirement l'une des deux options. *)
        let last_position = list_last_elem acc in

        (* On applique l'une des deux branches en fonction d'un tirage aléatoire. *)
        if Random.bool () then run_polymorphe transform p' last_position else run_polymorphe transform p'' last_position

    | _ -> acc @ []
  ) [i] program_V2  (* élément initial `i` et on applique chaque instruction du programme déplié. *)

(* Fonction qui applique un programme sur un rectangle et renvoie une approximation du rectangle résultant. *)
let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  let apply_transform t r =
    match t with
    | Translate v -> transform_rect (Translate v) r  (* applique une transformation de type `Translate` au rectangle. *)
    | Rotate (c, alpha) -> transform_rect (Rotate (c, alpha)) r  (* applique une transformation de type `Rotate` au rectangle. *)
  in
  let rec process prog r =
    match prog with
    | [] -> r  
    | Move t :: rest ->
        let new_rect = apply_transform t r in
        process rest new_rect  (* On applique le programme restant sur le nouveau rectangle. *)

    | Repeat (n, sub_prog) :: rest ->  (* si l'instruction est une répétition, on l'applique plusieurs fois. *)
        let rec repeat_apply n r acc =
          if n = 0 then acc
          else
            let new_r = process sub_prog r in  (* on applique le sous-programme sur le rectangle. *)
            repeat_apply (n - 1) new_r (List.rev_append acc [new_r])  (* on répète l'application du sous-programme. *)
        in
        let repeat_rects = repeat_apply n r [] in
        List.fold_left (fun acc r -> process rest r) r repeat_rects  (* on applique le reste du programme sur les rectangles générés par les répétitions. *)

    | Either (prog1, prog2) :: rest ->
        let rect1 = process prog1 r in
        let rect2 = process prog2 r in
        let x_min = min rect1.x_min rect2.x_min in
        let x_max = max rect1.x_max rect2.x_max in
        let y_min = min rect1.y_min rect2.y_min in
        let y_max = max rect1.y_max rect2.y_max in
        let new_rect = { x_min; x_max; y_min; y_max } in
        process rest new_rect  (* on applique le programme restant sur le rectangle obtenu après avoir pris le min et max des deux sous-programmes. *)
  in
  process prog r

(* Fonction qui vérifie si un programme atteint un rectangle cible `target`. Elle effectue une approximation du rectangle final 
   en utilisant `over_approximate` et vérifie si ce rectangle est inclus dans la cible. *)
let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
    let approximated_rect = over_approximate prog r in inclusion approximated_rect target
