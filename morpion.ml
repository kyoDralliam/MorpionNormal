open OcsfmlWindow
open OcsfmlGraphics


type joueur = Croix | Cercle 

type geometry = { position : float * float ; dimension : float }

type geometry_grille = { geometry : geometry ; inter : float ; side : float } 

type morpion = 
    Grille of morpion array array * geometry_grille * drawable 
  | Coup of joueur * geometry * drawable * drawable
  (*| TemporaryCoup of joueur * geometry * drawable * drawable*)
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

let access t c = let (x,y) = coords_from_morpion_case c in t.(x).(y)
let change t c a = let (x,y) = coords_from_morpion_case c in t.(x).(y) <- a

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


let rec draw (target : #render_target) ?blend_mode ?texture ?transform ?shader  = function 
  | Vide (_, img) -> target#draw ?blend_mode ?texture ?transform ?shader img
  | Coup (_, _, img, img0) -> 
      target#draw ?blend_mode ?texture ?transform ?shader img0 ; 
      target#draw ?blend_mode ?texture ?transform ?shader img
  | Grille (m, _, img) -> 
      target#draw ?blend_mode ?texture ?transform ?shader img ; 
      Array.iter (Array.iter (draw target ?blend_mode ?texture ?transform ?shader)) m
  

let position_coup_from_float pos geom entre cote = 
  let antipos x = 
    let ik = int_of_float (x /. entre) in 
    if 0 <= ik && ik < 28
    then [| -1 ; 0 ; 0 ; 0; 0; 0 ; 0 ; 0 ; 0; -1 ; 1; 1; 1; 1; 1; 1; 1; 1; -1; 2; 2; 2; 2; 2; 2; 2; 2; -1 |].(ik) 
    else -1
  in 
  antipos ((fst pos) -. (fst geom.position)), antipos ((snd pos) -. (snd geom.position))


let rec get_position_coup_base base pos = function 
  | Coup _ -> None
  | Vide _ -> Some []
  | Grille (m, { geometry ; inter ; side }, _) -> 
      let (x,y) = position_coup_from_float pos geometry inter side in 
      if x < 0 || x > 2 || y < 0 || y > 2 
      then base 
      else 
	match get_position_coup_base base pos m.(x).(y) with 
	  | None -> None 
	  | Some l -> Some ((morpion_case_from_coords x y) :: l)

let get_position_coup = get_position_coup_base (Some [])

let get_position_coup_vide = get_position_coup_base None

let rec get_content morpion path = 
    match path, morpion with 
    | [a], Grille (m, _, _) ->
	let (x,y) = coords_from_morpion_case a in 
	(match m.(x).(y) with 
	  | Vide (geom, img) -> (m, x, y, geom, img)
	  | _ -> assert false)
    | hd :: tl, Grille (m, _, _) -> get_content (access m hd) tl
    | _ -> assert false

let apply_morpion morpion path f = 
  let (m, x, y, geom, img) = get_content morpion path in 
  m.(x).(y) <- f geom img

let rec get_geometry morpion path = 
  match path, morpion with 
    | [], (Grille (_, {geometry ; _}, _) | Coup (_,geometry, _, _) | Vide (geometry, _)) -> geometry
    | hd :: tl, Grille (m, _, _) -> get_geometry (access m hd) tl
    | _ -> assert false


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


let victoire morpion l = 
  let rec get_grid = function 
    | [x], Grille (m, _, _) -> (m, x) 
    | x :: xs, Grille (m, _, _) -> get_grid (xs, (access m x))
    | _ -> assert false 
  in 
  let m, x = get_grid (l, morpion) in 
  let cmp a b = 
    match (access m a), (access m b) with 
      | Coup (j1, _, _, _), Coup (j2, _, _, _) -> j1 = j2
      | _ -> false 
  in 
  let is_same a b c = cmp a b && cmp b c in
  match x with 
      Center -> (is_same North Center South) || (is_same NorthEast Center SouthWest) || (is_same East Center West) || (is_same SouthEast Center NorthWest)
    | North -> (is_same North Center South) || (is_same NorthWest North NorthEast) 
    | NorthEast -> (is_same NorthWest North NorthEast) || (is_same NorthEast East SouthEast) || (is_same NorthEast Center SouthWest)
    | East -> (is_same NorthEast East SouthEast) || (is_same East Center West)
    | SouthEast -> (is_same NorthEast East SouthEast) || (is_same SouthEast Center NorthWest) || (is_same SouthEast South SouthWest)
    | South -> (is_same SouthEast South SouthWest) || (is_same North Center South)
    | SouthWest -> (is_same SouthEast South SouthWest) || (is_same NorthEast Center SouthWest) || (is_same SouthWest West NorthWest)
    | West -> (is_same SouthWest West NorthWest) || (is_same East Center West)
    | NorthWest -> (is_same NorthWest North NorthEast) || (is_same SouthEast Center NorthWest) || (is_same SouthWest West NorthWest)



let rec replace_case f morpion path = 
    match path, morpion with 
    | [a], Grille (m, _, _) ->
	let geom, img0 = match access m a with 
	  | Vide (geom, img0) -> geom, img0
	  | Coup (_, geom, _, img0) -> geom, img0
	  | Grille (_,{geometry; _},img0) -> geometry, img0
	in change m a (f geom img0)
    | hd :: tl, Grille (m, _, _) -> replace_case f (access m hd) tl
    | _ -> assert false


let remove_last l = List.(rev (tl (rev l)))

let rec process_victoire joueur morpion l = 
  if victoire morpion l 
  then 
    if List.length l = 1 
    then true (* victoire globale *)
    else (* il reste des niveaux au dessus *)
      let l' = remove_last l in 
      replace_case (creer_joueur joueur) morpion l' ;
      process_victoire joueur morpion l' 
  else false


let path_for_rect morpion rect = 
  let get = function Some x -> x | None -> [] in 
  let (&) f x = f x in
  let c1 = get & get_position_coup (rect.left, rect.top) morpion in 
  let c2 = get & get_position_coup (rect.left+.rect.width, rect.top) morpion in 
  let c3 = get & get_position_coup (rect.left, rect.top+.rect.height) morpion in 
  let c4 = get & get_position_coup (rect.left+.rect.width, rect.top+.rect.height) morpion in 
  let rec common_factor = function 
    | x::xs, y::ys when x = y -> x::(common_factor (xs,ys))
    | _, _ -> []
  in 
  common_factor (c1, common_factor (c2, common_factor (c3,c4)))

let draw_at_path (target : #render_target) ?blend_mode ?texture ?transform ?shader morpion chemin =
  let rec get_morpion = function 
    | [], x -> x
    | x::xs, Grille (m, _, _) -> get_morpion (xs, (access m x))
    | _, _ -> assert false
  in 
  draw target ?blend_mode ?texture ?transform ?shader (get_morpion (chemin, morpion)) ; 
  (8./.28.)**(float (List.length chemin))
  
  
