open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program

and program = instruction list

(* Fonction qui vérifie si un programme est déterministe, c'est-à-dire qu'il ne contient pas d'instructions `Either`. *)
let rec is_deterministic (prog : program) : bool =
  match prog with
  |[] -> true
  |hd::tl -> match hd with (* Parcours la liste d'instructions *)
    |Move _ -> is_deterministic tl
    |Repeat (_,repeat_prog) -> is_deterministic repeat_prog
    |Either _ -> false (* Si une instruction `Either` est rencontrée, le programme n'est pas déterministe. *)

(* Fonction qui déplie les instructions `Repeat` d'un programme et renvoie un programme sans `Repeat`. *)
let rec unfold_repeat (prog : program) : program =
  let rec prog_reap n repeat_prog =
    match n with
    | 0 -> []
    | n when n > 0 -> repeat_prog @ prog_reap (n - 1) repeat_prog
    | _ -> failwith "Le nombre de répétitions doit être positif"
  in
  match prog with
  | [] -> []
  | hd :: tl -> (* On traite chaque instruction du programme. *)
      match hd with
      | Move _ -> hd :: unfold_repeat tl (* on conserve et l'instruction et on déplie le reste du programme. *)

      | Repeat (n, repeat_prog) -> 
          prog_reap n repeat_prog @ unfold_repeat tl (* on déplie le programme répété et on continue avec le reste. *)

      | Either (prog1, prog2) ->
          Either (unfold_repeat prog1, unfold_repeat prog2) :: unfold_repeat tl (* on conserve les deux sous-programmes. *)


(* Fonction `list_last_elem` qui renvoie le dernier élément d'une liste et lève une exception si la liste est vide. *)
let rec list_last_elem lst =
  match lst with
  | [] -> failwith "Liste vide"
  | [x] -> x
  | _ :: t -> list_last_elem t


(* Fonction qui exécute un programme déterministe (`prog`) à partir d'un point initial `p`.
   Elle retourne la liste des points successifs générés par l'exécution des instructions. *)
let rec run_det (prog : program) (p : point) : point list =
  let program_V2 = unfold_repeat prog in  (* on déplie toutes les instructions `Repeat` du programme. *)
  List.fold_left
    (fun acc inst ->  (* on parcourt le programme et on applique chaque instruction à la liste des positions successives. *)
      match inst with
      | Move t ->
          let last_position = list_last_elem acc in  (* on récupère la dernière position de la liste. *)
          let new_position = transform t last_position in  (* on applique la transformation sur cette position. *)
          acc @ [new_position]  (* on ajoute la nouvelle position à la liste des positions. *)

      | _ -> failwith "Instruction Either ou Repeat rencontrée dans run_det"

    ) [p] program_V2  (* position initiale `p` et on applique chaque instruction du programme déplié. *)


(* Fonction qui vérifie si un programme déterministe atteint une cible. Elle exécute le programme à partir d'un point initial `p` 
   et vérifie si le dernier point atteint est dans le rectangle cible. *)
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  assert (is_deterministic prog);
  in_rectangle target (List.hd (List.rev (run_det prog p)))


(* Fonction qui exécute un programme (pas nécessairement déterministe) à partir d'un point `p`.
   Elle retourne la liste des points successifs générés par l'exécution des instructions. *)
let rec run (prog : program) (p : point) : point list =
  Random.self_init ();
  (* Déplier les répétitions pour obtenir un programme sans Repeat. *)
  let program_V3 = unfold_repeat prog in
  (* Utiliser List.fold_left pour parcourir toutes les instructions du programme déplié. *)
  List.fold_left (fun acc inst ->  
    match inst with
    (* Cas Move : appliquer une transformation à la dernière position et l'ajouter à la liste. *)
    | Move t -> 
        let last_position = list_last_elem acc in  (* Récupérer la dernière position dans acc. *)
        let new_position = transform t last_position in  (* Calculer la nouvelle position. *)
        acc @ [new_position]  (* Ajouter la nouvelle position à la liste acc. *)

    (* Cas Either : choisir aléatoirement un des deux sous-programmes. *)
    | Either (p1, p2) -> 
        let last_position = list_last_elem acc in  (* Récupérer la dernière position. *)
        let sub_program_result = 
          if Random.bool () then 
            (* Choix du premier sous-programme. *)
            run p1 last_position 
          else 
            (* Choix du second sous-programme. *)
            run p2 last_position
        in
        acc @ sub_program_result  (* Concaténer les points générés par le sous-programme avec acc. *)

    (* Cas non géré : lever une exception si un Repeat est encore présent (ne devrait pas arriver). *)
    | _ -> failwith "Instruction Repeat non dépliée"
  ) [p] program_V3  (* Initialiser la liste des positions avec le point de départ p. *)

(* Fonction qui génère toutes les combinaisons possibles d'exécutions pour un programme
   contenant des instructions `Either`. Elle déplie toutes les alternatives possibles du programme. *)
let rec all_choices (prog : program) : program list =
  let rec aux prog =
    match prog with
    | [] -> [[]]
    | Move t :: tl ->
        let rest_choices = aux tl in  (* on génère les choix pour le reste du programme. *)
        List.map (fun rest -> Move t :: rest) rest_choices  (* on ajoute l'instruction `Move` devant chaque possibilité. *)

    | Either (p1, p2) :: tl ->
        let choices_p1 = aux p1 in  (* on génère les choix pour le premier sous-programme. *)
        let choices_p2 = aux p2 in  (* on génère les choix pour le deuxième sous-programme. *)
        let rest_choices = aux tl in  (* on génère les choix pour le reste du programme. *)
        List.concat_map (fun choice1 ->
          List.map (fun rest -> choice1 @ rest) rest_choices
        ) (choices_p1 @ choices_p2)  (* On concatène toutes les combinaisons possibles. *)

    | Repeat (_, _) :: _ -> failwith "Les Repeat doivent être supprimés"
  in
  aux (unfold_repeat prog)  (* on applique la fonction auxiliare au programme déplié. *)

(* Fonction qui vérifie si un programme non déterministe atteint une cible.
   Elle exécute toutes les variantes possibles du programme et vérifie si l'une d'entre elles atteint la cible. *)
let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  assert (not (is_deterministic prog));
  let possible_execution_list = all_choices prog in
  List.for_all (fun sub_prog -> in_rectangle r (List.hd (List.rev (run sub_prog p)))) possible_execution_list
