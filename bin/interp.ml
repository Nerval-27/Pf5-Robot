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
  fc = (0, 0, 0);  (* Couleur de l'avant-plan par défaut : marron *)
  rc = (0, 0, 0);      (* Couleur du rectangle par défaut : noir *)
  pc = (0, 0, 0);      (* Couleur du point par défaut : noir *)
  size = (600, 400);   (* Taille de la fenêtre par défaut *)
  prog = 1;           (* Programme sé lectionné par défaut *)
  print= false;
}

(* Appliquer les couleurs à partir des options *)
let apply_colors opt_uses =
  (* Couleur de l'arrière-plan *)
  let (r, g, b) = opt_uses.bc in
  set_color (rgb r g b);
  fill_rect 0 0 (fst opt_uses.size) (snd opt_uses.size)

let show_user_order opt_uses=
    moveto ((fst opt_uses.size)-200) ((snd opt_uses.size)-100);
    let (r_f, g_f, b_f) = opt_uses.fc in
      set_color (rgb r_f g_f b_f); 
    draw_string "-PRESS S TO GO TO THE NEXT STEP";
    moveto ((fst opt_uses.size)-220) ((snd opt_uses.size)-50);
    draw_string "-PRESS P TO GO TO THE PREVIOUS STEP";
    moveto ((fst opt_uses.size)-250) ((snd opt_uses.size)-200)


let rec string_of_instruction instr =
  match instr with
  | Move (Translate {x; y}) -> Printf.sprintf "Move (Translate {x = %.2f; y = %.2f})" x y
  | Move (Rotate ({x; y}, angle)) -> Printf.sprintf "Move (Rotate ({x = %.2f; y = %.2f}, %.2f))" x y angle
  | Repeat (n, prog) -> Printf.sprintf "Repeat (%d, %s)" n (string_of_program (unfold_repeat prog))
  | Either (prog1, prog2) -> Printf.sprintf "Either (%s, %s)" (string_of_program prog1) (string_of_program prog2)

and string_of_program prog =
  String.concat "; " (List.map string_of_instruction prog)



(* Fonction pour convertir les coordonnées graphiques en graduations *)
let string_of_position x y  width_size height_size opt_uses=
let (r, g, b) = opt_uses.fc in
set_color (rgb r g b);
  let spacing_x = width_size / 25 in (* Espacement des graduations sur l'axe X *)
  let spacing_y = height_size / 25 in (* Espacement des graduations sur l'axe Y *)
  let origin_x = width_size / 2 in (* Centre de l'axe X *)
  let origin_y = height_size / 2 in (* Centre de l'axe Y *)

  let grad_x = ( x- origin_x) / spacing_x in
  let grad_y = ( y - origin_y) / spacing_y in
  moveto 10 ((snd opt_uses.size)-20);
  draw_string ("Pos of point ["^string_of_int grad_x ^ ", " ^ string_of_int grad_y^"]")


  (* Fonction pour convertir les coordonnées des rectangles en graduations *)
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

  (* Affichage des positions *)
  moveto 10 ((snd opt_uses.size) - 40);
  draw_string ("Pos of rect ["^"x_min: " ^ string_of_int grad_x_min ^ ", y_min: " ^ string_of_int grad_y_min
  ^ "x_max: "^string_of_int grad_x_max ^ ", y_max: " ^ string_of_int grad_y_max ^ "]")



(* Dessiner un rectangle *)
let draw_rectangle r opt_uses =
  let x = int_of_float r.x_min in
  let y = int_of_float r.y_min in
  let width = int_of_float (r.x_max -. r.x_min) in
  let height = int_of_float (r.y_max -. r.y_min) in
  let (r_c, g_c, b_c) = opt_uses.rc in
  set_color (rgb r_c g_c b_c);
  fill_rect x y width height




let porg_1 = [
  Repeat (30, [
    Move (Translate {x = 5.; y = 5.});
  ])
]


let prog_2= [
  Repeat (5, [
    Either (
      [Move (Translate {x = 1.; y = 0.})],
      [Move (Translate {x = 0.; y = 1.})]
    );
    Either (
      [Move (Translate {x = -1.; y = 0.})],
      [Move (Translate {x = 0.; y = -1.})]
    )
  ])
]


let  prog_3: program = [
  Move (Translate {x = 2.0; y = 0.0});
  Repeat (4, [
    Move (Rotate ({x = 0.0; y = 0.0}, 90.0));
    Move (Translate {x = 1.0; y = 1.0});
  ]);
  Either (
    [
      Repeat (3, [
        Move (Rotate ({x = 2.0; y = 2.0}, 45.0));
        Move (Translate {x = 0.0; y = 2.0});
      ])
    ],
    [
      Move (Translate {x = -2.0; y = -2.0});
      Repeat (2, [
        Move (Translate {x = 1.0; y = 0.0});
        Move (Rotate ({x = 0.0; y = 0.0}, -90.0));
      ])
    ]
  );
  Move (Translate {x = 3.0; y = -3.0});
]

let prog_list = [porg_1 ;  prog_2 ;  prog_3]

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

  Printf.printf "INST : %d\n" total_instruction;
  Printf.printf "Points : %d\n" total_steps_dot;
  Printf.printf "Rectangles : %d\n" total_steps_rect;
  flush stdout;

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
        (!actuel_str)
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