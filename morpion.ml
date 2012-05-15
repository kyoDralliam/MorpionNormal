open OcsfmlWindow
open OcsfmlGraphics


type joueur = Croix | Cercle 

type geometry = { position : float * float ; dimension : float }

type geometry_grille = { geometry : geometry ; inter : float ; side : float } 

type morpion = 
    Grille of morpion array array * geometry_grille * drawable 
  | Coup of joueur * geometry * drawable * drawable
  | Vide of geometry * drawable

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

(* faut ptet arréter de faire l'idiot... 
module String = struct
    let get t c = let (x,y) = coords_from_morpion_case c in t.(x).(y)
    let set t c a = let (x,y) = coords_from_morpion_case c in t.(x).(y) <- a
end
 *)
let morpion_case_from_coords x y = 
  match x with  
    | 0 -> 
	begin match y with 
	  | 0 -> NorthWest
	  | 1 -> West
	  | 2 -> SouthWest
	  | _ -> assert false
	end
    | 1 ->
	begin match y with 
	  | 0 -> North
	  | 1 -> Center
	  | 2 -> South
	  | _ -> assert false
	end
    | 2 ->
	begin match y with 
	  | 0 -> NorthEast
	  | 1 -> East
	  | 2 -> SouthEast
	  | _ -> assert false
	end
    | _ -> assert false

type chemin = morpion_case list 

let creer_grille geometry =
  let inter = geometry.dimension /. 28. in
  let dimension = 8. *. inter in

  let carre = new rectangle_shape  
    ~position:geometry.position 
    ~size:(geometry.dimension, geometry.dimension)
    ~fill_color:(Color.rgb 128 128 128) 
    ~outline_color:Color.black 
    ~outline_thickness:(min 2.0 (geometry.dimension/.20.)) () in  
 
  let pos k = float k *. (dimension +. inter) +. inter in
  let creer_vide i j = 
    let position = fst geometry.position +. pos i, snd geometry.position +. pos j in 
    let petit_carre = new rectangle_shape  
      ~position 
      ~size:(dimension, dimension) 
      ~fill_color:Color.white
      ~outline_color:Color.black
      ~outline_thickness:(min 2.0 (dimension/.20.)) () 
    in  
    Vide ({ position ; dimension }, (petit_carre :> drawable)) 
  in  Grille (Array.init 3 (fun i -> Array.init 3 (creer_vide i)), { geometry ; inter ; side = dimension }, (carre :> drawable))


let creer_cercle { position ; dimension } =
  let position = (fst position) +. (dimension/.4.), (snd position) +. (dimension/.4.) in 
  let circle = new circle_shape  
    ~radius:(dimension/.4.) 
    ~position 
    ~fill_color:Color.red
    ~outline_color:Color.black 
    ~outline_thickness:(min 2.0 (dimension/.20.)) ()
  in (circle :> drawable)

let creer_croix { position ; dimension } = 
  let position = (fst position) +. (dimension/.4.), (snd position) +. (dimension/.4.) in
  let carre = new rectangle_shape  
    ~size:(dimension/.2.,dimension/.2.) 
    ~position 
    ~fill_color:Color.blue
    ~outline_color:Color.black 
    ~outline_thickness:(min 2.0 (dimension/.20.)) ()
  in (carre :> drawable)

let creer_joueur joueur geom img0 =
  let img = 
    match joueur with 
      | Croix -> creer_croix geom 
      | Cercle -> creer_cercle geom
  in 
  Coup (joueur, geom, img, img0)


let rec draw (target : #render_target) = function 
  | Vide (_, img) -> target#draw img
  | Coup (_, _, img, img0) -> target#draw img0 ; target#draw img
  | Grille (m, _, img) -> target#draw img ; Array.iter (Array.iter (draw target)) m


let position_coup_from_float pos geom entre cote = 
  let antipos x = int_of_float ((x -. entre) /. (cote +. entre)) in 
  antipos ((fst pos) -. (fst geom.position)), antipos ((snd pos) -. (snd geom.position))


let rec get_position_coup pos = function 
  | Coup _ -> None
  | Vide _ -> Some []
  | Grille (m, { geometry ; inter ; side }, _) -> 
      let (x,y) = position_coup_from_float pos geometry inter side in 
      if x < 0 || x > 2 || y < 0 || y > 2 
      then None 
      else 
	match get_position_coup pos m.(x).(y) with 
	  | None -> None 
	  | Some l -> Some ((morpion_case_from_coords x y) :: l)

let rec apply_morpion morpion path f = 
  match path, morpion with 
    | [a], Grille (m, _, _) ->
	let (x,y) = coords_from_morpion_case a in 
	(match m.(x).(y) with 
	  | Vide (geom, img) -> m.(x).(y) <- f geom img
	  | _ -> assert false)
    | hd :: tl, Grille (m, _, _) -> 
	let (x,y) = coords_from_morpion_case hd in 
	apply_morpion m.(x).(y) tl f
    | _ -> assert false

let jouer_coup pos_joueur_cercle pos_joueur_croix morpion =
  if pos_joueur_cercle = pos_joueur_croix
  then apply_morpion morpion pos_joueur_croix (fun geom _ -> creer_grille geom)
  else 
    begin 
      apply_morpion morpion pos_joueur_croix (creer_joueur Croix) ;
      apply_morpion morpion pos_joueur_cercle (creer_joueur Cercle)
    end

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

let print_position_coup = function  
  | None -> print_endline "pas de coup"
  | Some l -> print_endline (String.concat ", " (List.map string_of_morpion_case l))
