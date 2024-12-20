let spirale_carre = [
  Repeat (10, [
    Move (Translation {x = 1.; y = 0.});
    Move (Rotation ({x = 0.; y = 0.}, 90.));
    Move (Translation {x = 2.; y = 0.});
    Move (Rotation ({x = 0.; y = 0.}, 90.));
  ])
]



let exploration = [
  Repeat (5, [
    Either (
      [Move (Translation {x = 1.; y = 0.})],
      [Move (Translation {x = 0.; y = 1.})]
    );
    Either (
      [Move (Translation {x = -1.; y = 0.})],
      [Move (Translation {x = 0.; y = -1.})]
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
      

let prog_list=spirale_carre::exploration::complex_program::[]





let rec loop prog opt_uses =
  let dot_list=run opt_uses.prog ({x=0.;y=0.}) in
  let rect_list= match opt_uses.abs with
  |None,_-> []
  |Some (rec),_ -> run_rect opt_uses.prog rec

in 
  let rect= match rect_list with
  |[] -> ()
  |h::q -> get_index_list result_run !actuel 
in 
    let dot=get_index_list result_dot !actuel 
in 
match (snd opt_uses.abs),opt_uses.cr with
|(_,true),true -> draw_rectangle rec; plot dot;
|(_,false),_ -> plot dot;
|(_,true),false -> draw_rectangle rec;
in 
    let event = wait_next_event [Key_pressed] in
    if event.keypressed then
      match event.key with
      | 's' -> 
          if !actuel + 1 < total_steps then incr actuel;
          loop ()
      | 'r' -> (* Étape précédente *)
          if !actuel > 0 then decr actuel; 
          loop ()
      | 'q' -> raise Quit
      | _ -> loop ()
    else loop ()
  in
  loop ()
