(* Code de la Section 3 du projet. *)

type coord2D = {
    x : float;
    y : float
  }
type point = coord2D
type vector = coord2D
type angle = float

(* Fonction de translation : déplace un point p par un vecteur v. *)
let translate (v : vector) (p : point) : point =
  {x = p.x +. v.x ; y = p.y +. v.y}

(* Conversion d'un angle de degrés en radians. *)
let rad_of_deg (a : angle) : angle =
  a *. (Float.pi /. 180.0)

(* Conversion d'un angle de radians en degrés. *)
let deg_of_rad (a : angle) : angle =
  a *. (180.0 /. Float.pi)

(* Fonction de rotation d'un point p autour d'un point c d'un angle alpha (en degrés). *)
let rotate (c : point) (alpha : angle) (p : point) : point =
  let theta = rad_of_deg alpha in
  {x = c.x +. (p.x -. c.x) *. (Float.cos theta) -. (p.y -. c.y) *. (Float.sin theta) ;
   y = c.y +. (p.x -. c.x) *. (Float.sin theta) +. (p.y -. c.y) *. (Float.cos theta)}
  
type transformation =
  Translate of vector
| Rotate of point * angle

(* Applique une transformation t sur un point p. *)
let transform (t : transformation) (p : point) : point =
  match t with
  |Translate v -> translate v p
  |Rotate (c,alpha) -> rotate c alpha p

type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
  }

(* Vérifie si un point p est à l'intérieur d'un rectangle r. *)
let in_rectangle (r : rectangle) (p : point) : bool =
  (r.x_min <= p.x && r.x_max >= p.x) && (r.y_min <= p.y && r.y_max >= p.y)

(* Retourne la liste des 4 coins du rectangle r. *)
let corners (r :rectangle) : point list =
  [{x = r.x_min ; y = r.y_min} ; {x = r.x_min ; y = r.y_max} ; {x = r.x_max ; y = r.y_min} ; {x = r.x_max ; y = r.y_max}]

(* Fonction qui génère le plus petit rectangle englobant tous les points d'une liste. *)
let rectangle_of_list (pl : point list) : rectangle =
  match pl with
  | [] -> failwith "La liste de points ne peut pas être vide"
  | p0 :: reste ->
      let x_min, x_max, y_min, y_max =
        List.fold_left (fun (x_min, x_max, y_min, y_max) p ->
          (* On parcourt les points et on met à jour les bornes minimales et maximales. *)
          (Float.min x_min p.x, Float.max x_max p.x, Float.min y_min p.y, Float.max y_max p.y)
        ) (p0.x, p0.x, p0.y, p0.y) reste
      in
      { x_min; x_max; y_min; y_max }
