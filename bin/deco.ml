open Graphics

(* Fonction pour dessiner les axes avec graduations *)
let fun_draw_axe_x width_size height_size =
  let rec aux acc spacing_x = 
    (* Axe horizontal *)
    moveto 0 (height_size / 2);
    lineto width_size (height_size / 2);
    match acc with
    | 0 -> () (* Arrêt de la récursion *)
    | _ -> 
      (* Dessin des graduations sur l'axe horizontal *)
      let x = (width_size / 2) + (acc * spacing_x) in
      let x_neg = (width_size / 2) - (acc * spacing_x) in
      moveto x ((height_size / 2) - 5);
      lineto x ((height_size / 2) + 5);
      moveto (x - 10) ((height_size / 2) + 10);
      draw_string (string_of_int (acc * spacing_x));
      moveto x_neg ((height_size / 2) - 5);
      lineto x_neg ((height_size / 2) + 5);
      moveto (x_neg - 10) ((height_size / 2) + 10);
      draw_string (string_of_int (-acc * spacing_x));
      aux (acc - 1) spacing_x (* Appel récursif *)
  in
  aux (width_size / (width_size / 5)) (width_size / 5)


  let fun_draw_axe_y width_size height_size =
    let rec aux acc spacing_y = 
      (* Axe vertical *)
      moveto (width_size / 2) 0;
      lineto (width_size / 2) height_size;
      match acc with
      | 0 -> () (* Arrêt de la récursion *)
      | _ -> 
        (* Dessin des graduations sur l'axe vertical *)
        let y = (height_size / 2) + (acc * spacing_y) in
        let y_neg = (height_size / 2) - (acc * spacing_y) in
        moveto ((width_size / 2) - 5) y;
        lineto ((width_size / 2) + 5) y;
        moveto ((width_size / 2) + 10) (y - 5);
        draw_string (string_of_int (acc * spacing_y));
        moveto ((width_size / 2) - 5) y_neg;
        lineto ((width_size / 2) + 5) y_neg;
        moveto ((width_size / 2) + 10) (y_neg - 5);
        draw_string (string_of_int (-acc * spacing_y));
        aux (acc - 1) spacing_y (* Appel récursif *)
    in
    aux (height_size / (height_size / 5)) (height_size / 5)



