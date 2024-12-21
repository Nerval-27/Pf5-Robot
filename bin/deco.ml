open Graphics
open Parse

(* Fonction pour dessiner les axes avec graduations *)
let fun_draw_axe_x width_size height_size opt_uses =
  let (r_f, g_f, b_f) = opt_uses.fc in
  set_color (rgb r_f g_f b_f); 
  let spacing_x = width_size / 25 in (* Espacement des graduations sur l'axe X *)
  let max_grad_x = width_size / 2 in (* Position centrale sur l'axe X *)
  let rec aux acc =
    (* Axe horizontal *)
    moveto 0 (height_size / 2);
    lineto width_size (height_size / 2);
    if acc > 0 then begin
      (* Dessin des graduations sur l'axe horizontal *)
      let x = max_grad_x + (acc * spacing_x) in
      let x_neg = max_grad_x - (acc * spacing_x) in
      (* Graduation positive *)
      moveto x ((height_size / 2) - 5);
      lineto x ((height_size / 2) + 5);
      moveto (x - 1) ((height_size / 2) + 10);
      draw_string (string_of_int (acc));
      (* Graduation négative *)
      moveto x_neg ((height_size / 2) - 5);
      lineto x_neg ((height_size / 2) + 5);
      moveto (x_neg - 8) ((height_size / 2) + 10);
      draw_string (string_of_int (-acc));
      aux (acc - 1)
    end
  in
  aux (width_size / (2 * spacing_x))

let fun_draw_axe_y width_size height_size opt_uses =
  let (r_f, g_f, b_f) = opt_uses.fc in
  set_color (rgb r_f g_f b_f); 
  let spacing_y = height_size / 25 in (* Espacement des graduations sur l'axe Y *)
  let max_grad_y = height_size / 2 in (* Position centrale sur l'axe Y *)
  let rec aux acc =
    (* Axe vertical *)
    moveto (width_size / 2) 0;
    lineto (width_size / 2) height_size;
    if acc > 0 then begin
      (* Dessin des graduations sur l'axe vertical *)
      let y = max_grad_y + (acc * spacing_y) in
      let y_neg = max_grad_y - (acc * spacing_y) in
      (* Graduation positive *)
      moveto ((width_size / 2) - 5) y;
      lineto ((width_size / 2) + 5) y;
      moveto ((width_size / 2) + 10) (y - 5);
      draw_string (string_of_int (acc));
      (* Graduation négative *)
      moveto ((width_size / 2) - 5) y_neg;
      lineto ((width_size / 2) + 5) y_neg;
      moveto ((width_size / 2) + 10) (y_neg - 5);
      draw_string (string_of_int (-acc));
      aux (acc - 1)
    end
  in
  aux (height_size / (2 * spacing_y))