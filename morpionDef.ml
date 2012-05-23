type joueur = Croix | Cercle 

type morpion = 
    Grille of morpion array array
  | Coup of joueur 
  | CoupTmp of joueur 
  | Vide

type morpion_case = 
    Center 
  | North 
  | NorthEast 
  | East 
  | SouthEast 
  | South 
  | SouthWest 
  | West 
  | NorthWest 

let coords_from_morpion_case = function 
    Center -> 1, 1
  | North -> 1, 0
  | NorthEast -> 2, 0
  | East -> 2, 1
  | SouthEast -> 2, 2
  | South -> 1, 2
  | SouthWest -> 0, 2
  | West -> 0, 1
  | NorthWest -> 0, 0

let access t c = let (x,y) = coords_from_morpion_case c in t.(x).(y)
let change t c a = let (x,y) = coords_from_morpion_case c in t.(x).(y) <- a
let modify t c f = let (x,y) = coords_from_morpion_case c in t.(x).(y) <- f t.(x).(y)

(* faut ptet arréter de faire l'idiot... 
module String = struct
    let get t c = let (x,y) = coords_from_morpion_case c in t.(x).(y)
    let set t c a = let (x,y) = coords_from_morpion_case c in t.(x).(y) <- a
end
 *)
let morpion_case_from_coords = 
  let conversion =
    [|
      [| NorthWest ; North  ; NorthEast |] ;
      [| West      ; Center ; East      |] ;
      [| SouthWest ; South  ; SouthEast |] 
    |]
  in fun x y -> conversion.(y).(x)

type chemin = morpion_case list 

let string_of_morpion_case =  function 
    Center -> "Center"
  | North -> "North"
  | NorthEast -> "NorthEast"
  | East -> "East"
  | SouthEast -> "SouthEast"
  | South -> "South"
  | SouthWest -> "SouthWest"
  | West -> "West"
  | NorthWest -> "NorthWest"

let creer_grille () = Grille (Array.make_matrix 3 3 Vide) 
let creer_joueur j = Coup j
let creer_joueur_tmp j = CoupTmp j
