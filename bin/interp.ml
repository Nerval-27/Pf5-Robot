open Graphics;;
open Pf5;;
open Geo;;
open Interp;;
open Approx;;
open Parse;;
exception Quit;;

let default_option () = {
  abs = (None, false);
  cr = false;
  bc = (139, 69, 19);
  fc = (139, 69, 19);
  rc = (0, 0, 0);
  pc = (0, 0, 0);
  size = (600, 400);
  prog = 1;
}

let draw_rectangle r =
  let x = int_of_float r.x_min in
  let y = int_of_float r.y_min in
  let width = int_of_float (r.x_max -. r.x_min) in
  let height = int_of_float (r.y_max -. r.y_min) in
  set_color (rgb 0 255 0); (* Couleur verte pour les rectangles *)
  draw_rect x y width height

let spirale_carre = [
  Repeat (10, [
    Move (Translate {x = 1.; y = 0.});
    Move (Rotate({x = 0.; y = 0.}, 90.));
    Move (Translate {x = 2.; y = 0.});
    Move (Rotate({x = 0.; y = 0.}, 90.));
  ])
]

let exploration = [
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

let complex_program : program = [
  Move (Translate {x = 2.0; y = 0.0});  (* Déplacement initial *)
  Repeat (4, [
    Move (Rotate ({x = 0.0; y = 0.0}, 90.0));  (* Rotation autour de l'origine *)
    Move (Translate {x = 1.0; y = 1.0});      (* Translation diagonale *)
  ]);
  Either (
    [
      Repeat (3, [
        Move (Rotate ({x = 2.0; y = 2.0}, 45.0));  (* Rotation autour d'un point *)
        Move (Translate {x = 0.0; y = 2.0});      (* Translation verticale *)
      ])
    ],
    [
      Move (Translate {x = -2.0; y = -2.0});  (* Retour en arrière *)
      Repeat (2, [
        Move (Translate {x = 1.0; y = 0.0});  (* Translation horizontale *)
        Move (Rotate ({x = 0.0; y = 0.0}, -90.0));  (* Rotation négative *)
      ])
    ]
  );
  Move (Translate {x = 3.0; y = -3.0});  (* Grand déplacement diagonal *)
  Repeat (5, [
    Either (
      [
        Move (Rotate ({x = 1.0; y = 1.0}, 30.0));
        Move (Translate {x = 0.5; y = 0.5});
      ],
      [
        Move (Translate {x = -0.5; y = -0.5});
        Move (Rotate ({x = -1.0; y = -1.0}, -30.0));
      ]
    )
  ])
]

let prog_list = spirale_carre :: exploration :: complex_program :: []

let draw_state rect_opt point_opt =
  (match rect_opt with
   | Some rect -> draw_rectangle rect
   | None -> ());
  (match point_opt with
   | Some point -> set_color (rgb 255 0 0); (* Couleur rouge pour les points *)
     plot (int_of_float point.x) (int_of_float point.y)
   | None -> ())

let execute opt_uses =
  let dot_list = run (List.nth prog_list opt_uses.prog) {x = 0.; y = 0.} in
  let rect_list = match opt_uses.abs with
    | None, _ -> []
    | Some rect, _ -> run_rect (List.nth prog_list opt_uses.prog) rect
  in
  let actuel = ref 0 in
  let total_steps = List.length dot_list in

  let rec loop () =
    let rect = if snd opt_uses.abs then Some (List.nth rect_list !actuel) else None in
    let dot = Some (List.nth dot_list !actuel) in
    draw_state rect (if opt_uses.cr then dot else None);

    let event = wait_next_event [Key_pressed] in
    if event.keypressed then
      match event.key with
      | 's' -> if !actuel + 1 < total_steps then incr actuel; loop ()
      | 'r' -> if !actuel > 0 then decr actuel; loop ()
      | 'q' -> raise Quit
      | _ -> loop ()
    else loop ()
  in
  loop ()

let () =
  try
    let all_info = parse Sys.argv 1 (default_option ()) in
    let (width, height) = all_info.size in
    open_graph (Printf.sprintf " %dx%d" width height);
    execute all_info;
  with Quit -> close_graph ();;
