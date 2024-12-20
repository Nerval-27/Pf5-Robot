(* Déclaration du type 'opt' avec tous ses champs *)
type opt = {
  abs: Pf5.Geo.rectangle option * bool;  
  cr: bool;
  bc: int * int * int; 
  fc: int * int * int;  
  rc: int * int * int; 
  pc: int * int * int;  
  size: int * int;  
  prog: int;  
}


(* Fonctions pour la conversion de chaînes en types numériques *)
val convert_of_float : string -> float option
val convert_of_int : string -> int option

(* Déclaration de la fonction 'parse' pour analyser les arguments de la ligne de commande *)
val parse : string array -> int -> opt -> opt