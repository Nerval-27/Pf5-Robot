open Graphics;;
open Pf5;;
open Geo;;
open Interp;;
open Approx;;
open Deco;;
open Parse;;
exception Quit;;

(* Configuration par défaut des options *)
let default_option () = {
  abs = (None, false);
  cr = false;
  bc = (139, 69, 19);  (* Couleur de fond par défaut : marron *)
  fc = (0, 0, 0);  
  rc = (0, 0, 0);      (* Couleur du rectangle par défaut : noir *)
  pc = (0, 0, 0);      (* Couleur du point par défaut : noir *)
  size = (600, 400);   (* Taille de la fenêtre par défaut *)
  prog = 1;           (* Programme selectionné par défaut *)
  print= false;
}

(* Appliquer les couleurs à partir des options *)
let apply_colors opt_uses =
  let (r, g, b) = opt_uses.bc in
  set_color (rgb r g b);
  fill_rect 0 0 (fst opt_uses.size) (snd opt_uses.size)

  (* Affiche les commandes utilisateur à l'écran *)
let show_user_order opt_uses=
    moveto ((fst opt_uses.size)-(fst opt_uses.size)/3) ((snd opt_uses.size)-35);
    let (r_f, g_f, b_f) = opt_uses.fc in
      set_color (rgb r_f g_f b_f); 
    draw_string "-PRESS S TO GO TO THE NEXT STEP";
    moveto ((fst opt_uses.size)-(fst opt_uses.size)/3) ((snd opt_uses.size)-55);
    draw_string "-PRESS P TO GO TO THE PREVIOUS STEP";
    moveto ((fst opt_uses.size)-(fst opt_uses.size)/3) ((snd opt_uses.size)-75);
    draw_string "-PRESS Q TO QUIT";
    moveto ((fst opt_uses.size)-(fst opt_uses.size)/3) ((snd opt_uses.size)-95)


    let truncate_string (s : string) (max_length : int) : string =
      if String.length s > max_length then
        String.sub s 0 max_length ^ "..."
      else
        s
   
    let rec string_of_instruction instr =
      match instr with
      | Move (Translate {x; y}) -> 
          Printf.sprintf "Move (Translate {x = %.2f; y = %.2f})" x y
      
      | Move (Rotate ({x; y}, angle)) -> 
          Printf.sprintf "Move (Rotate ({x = %.2f; y = %.2f}, %.2f))" x y angle
      
      | Repeat (n, prog) -> 
          Printf.sprintf "Repeat (%d, %s)" n (string_of_program (unfold_repeat prog))
      
      | Either (prog1, prog2) -> 
          let prog1_str = truncate_string (string_of_program prog1) 15 in
          let prog2_str = truncate_string (string_of_program prog2) 15 in
          Printf.sprintf "Either (%s, %s)" prog1_str prog2_str
    
    and string_of_program prog =
      String.concat "; " (List.map string_of_instruction prog)
    


(* Affiche les coordonnées d'un point sur la fenêtre graphique *)
let string_of_position x y  width_size height_size opt_uses=
let (r, g, b) = opt_uses.fc in
set_color (rgb r g b);
  let spacing_x = width_size / 25 in 
  let spacing_y = height_size / 25 in 
  let origin_x = width_size / 2 in 
  let origin_y = height_size / 2 in 

  let grad_x = (x- origin_x) / spacing_x in
  let grad_y = ( y - origin_y) / spacing_y in
  moveto 10 (height_size - (height_size/5) ); 
  draw_string ("Pos of point ["^string_of_int grad_x ^ ", " ^ string_of_int grad_y^"]")

(* Affiche les coordonnées d'un rectangle sur la fenêtre graphique *)
let string_of_rectangle_position rect width_size height_size opt_uses =
  let (r, g, b) = opt_uses.rc in
  set_color (rgb r g b);
  let spacing_x = width_size / 25 in (* Espacement des graduations sur l'axe X *)
  let spacing_y = height_size / 25 in (* Espacement des graduations sur l'axe Y *)
  let origin_x = width_size / 2 in (* Centre de l'axe X *)
  let origin_y = height_size / 2 in (* Centre de l'axe Y *)

  let grad_x_min = (int_of_float rect.x_min - origin_x) / spacing_x in
  let grad_y_min = (int_of_float rect.y_min - origin_y) / spacing_y in
  let grad_x_max = (int_of_float rect.x_max - origin_x) / spacing_x in
  let grad_y_max = (int_of_float rect.y_max - origin_y) / spacing_y in

  moveto 10 ((snd opt_uses.size) - 40);
  draw_string ("Pos of rect [x_min: " ^ string_of_int grad_x_min ^ ", y_min: ");
  moveto 10 (height_size - 60);  (* Ligne 2 *)
  draw_string (" " ^ string_of_int grad_y_min ^ ", x_max: " ^ string_of_int grad_x_max ^ ", y_max: " ^ string_of_int grad_y_max ^ "]")


(* Dessiner un rectangle *)
let draw_rectangle r opt_uses =
  let x = int_of_float r.x_min in
  let y = int_of_float r.y_min in
  let width = int_of_float (r.x_max -. r.x_min) in
  let height = int_of_float (r.y_max -. r.y_min) in
  let (r_c, g_c, b_c) = opt_uses.rc in
  set_color (rgb r_c g_c b_c);
  fill_rect x y (width*((fst opt_uses.size)/25)) (height*((snd opt_uses.size)/25))
  

(* Dessiner l'état courant du robot *)
let draw_state rect_opt point_opt opt_uses =
  (* Dessiner le rectangle si présent *)
  (match rect_opt with
   | Some rect -> 
       draw_rectangle rect opt_uses;
       string_of_rectangle_position rect (fst opt_uses.size) (snd opt_uses.size) opt_uses
   | None -> ());
  (* Dessiner le point si présent *)
  (match point_opt with
   | Some point -> 
       string_of_position (int_of_float point.x) (int_of_float point.y) (fst opt_uses.size) (snd opt_uses.size) opt_uses;
       let (r_p, g_p, b_p) = opt_uses.pc in
       set_color (rgb r_p g_p b_p);
       fill_circle (int_of_float point.x) (int_of_float point.y) 5
   | None -> ())


(* Exécute le programme *)
let execute opt_uses =
  let taille_graduationX = (float_of_int ((fst opt_uses.size)/25)) in
  let taille_graduationY = float_of_int ((snd opt_uses.size)/25) in
  let prog_1 : program = [
    Repeat (9, [
      Move (Translate {x = taille_graduationX ; y = taille_graduationY});
  ])
  ] in
  
  let prog_2: program = [
    Either (
      [
        Move (Translate {x = taille_graduationX; y = taille_graduationY});
        Repeat (3, [
          Move (Translate {x = 0.; y = taille_graduationY})
        ]);

        Repeat (8, [
          Move (Translate {x = taille_graduationX; y = 0.})
        ])

      ],
      [
        Move (Translate {x = taille_graduationX; y = taille_graduationY});
        Repeat (4, [
          Move (Translate {x = taille_graduationX; y = 0.})
        ]);
        Repeat (6, [
          Move (Translate {x = 0.; y = taille_graduationY})
        ])
      ]
    )
  ] in 
  
  let prog_3 : program = [
    Repeat (8, [
      Move (Translate {x = taille_graduationX; y = taille_graduationY})
    ]);

    Repeat (5, [
      Move (Translate {x = taille_graduationX; y = -. taille_graduationY})
    ]);

    Repeat (5, [
      Move (Translate {x = -. taille_graduationX; y = 0.})
    ])
  ] in 

  let prog_list = [prog_1 ;  prog_2 ;  prog_3] in
  let unfolded_prog = unfold_repeat (List.nth prog_list (opt_uses.prog - 1)) in
  let dot_list = run (List.nth prog_list (opt_uses.prog - 1)) {x = 0.; y = 0.} in
  let rect_list = match opt_uses.abs with
    | None, _ -> []
    | Some rect, _ -> if not (in_rectangle rect {x = 0.; y = 0.}) then raise Quit
      else run_rect (List.nth prog_list (opt_uses.prog - 1)) rect
  in
  let actuel_dot = ref 0 in
  let actuel_rect = ref 0 in
  let actuel_str = ref 0 in 
  let total_steps_dot = List.length dot_list in
  let total_steps_rect = List.length rect_list in
  let total_instruction= List.length unfolded_prog in

  let rec loop () =
    let rect = if snd opt_uses.abs then Some (List.nth rect_list !actuel_rect) else None in
    let dot = Some (List.nth dot_list !actuel_dot) in
    clear_graph ();
    apply_colors opt_uses;
    show_user_order opt_uses;
    fun_draw_axe_x (fst opt_uses.size) (snd opt_uses.size) opt_uses;
    fun_draw_axe_y (fst opt_uses.size) (snd opt_uses.size) opt_uses;
    draw_state rect (if (snd opt_uses.abs) && not opt_uses.cr then None else dot) opt_uses;

    (* Affichage de l'instruction si print est activé *)
    if opt_uses.print then
      Printf.printf "Étape %d : %s\n"
        (!actuel_dot)
        (string_of_instruction (List.nth unfolded_prog !actuel_str));
    flush stdout;

    let event = wait_next_event [Key_pressed] in
    if event.keypressed then
      match event.key with
      | 's' -> if !actuel_dot + 1 < total_steps_dot then incr actuel_dot;
               if !actuel_rect + 1 < total_steps_rect then incr actuel_rect;
               if !actuel_str +1< total_instruction then incr actuel_str;
               loop ()
      | 'p' -> if !actuel_dot > 0 then decr actuel_dot;
               if !actuel_rect > 0 then decr actuel_rect;
               if !actuel_str > 0 then decr actuel_str;
               loop ()
      | 'q' -> raise Quit
      | _ -> loop ()
    else loop ()
  in
  loop ()


(* Point d'entrée principal *)
let () =
  try
    let all_info = parse Sys.argv 1 (default_option ()) in
    let (width, height) = all_info.size in
    open_graph (Printf.sprintf " %dx%d" width height);
    apply_colors all_info;
    show_user_order all_info;
    fun_draw_axe_x (fst all_info.size) (snd all_info.size) all_info;
    fun_draw_axe_y (fst all_info.size) (snd all_info.size) all_info;
    execute all_info;
  with Quit -> close_graph ();;