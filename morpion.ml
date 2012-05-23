open OcsfmlGraphics
open MorpionDef
open MorpionDraw

module DrawFunctionImplementation : DrawFunction =
struct

  let draw_grid (target : #render_target) ?blend_mode ?texture ?transform ?shader depth pos0 dim0 =
    let carre = new rectangle_shape  
      ~position:pos0 
      ~size:(dim0, dim0)
      ~fill_color:(Color.rgb 128 128 128) 
      ~outline_color:Color.black 
      ~outline_thickness:(min 2.0 (dim0/.200.)) () 
    in  
    target#draw ?blend_mode ?texture ?transform ?shader carre
    

  let draw_vide (target : #render_target) ?blend_mode ?texture ?transform ?shader depth pos0 dim0 =
    let petit_carre = new rectangle_shape  
      ~position:pos0
      ~size:(dim0, dim0) 
      ~fill_color:Color.white
      ~outline_color:Color.black
      ~outline_thickness:(min 2.0 (dim0/.200.)) () 
    in  
    target#draw ?blend_mode ?texture ?transform ?shader petit_carre


  let draw_cercle (target : #render_target) ?blend_mode ?texture ?transform ?shader depth pos0 dim0 fill_color =
    let position = (fst pos0) +. (dim0/.4.), (snd pos0) +. (dim0/.4.) in 
    let circle = new circle_shape  
      ~radius:(dim0/.4.) 
      ~position 
      ~fill_color
      ~outline_color:Color.black 
      ~outline_thickness:(min 2.0 (dim0/.200.)) ()
    in 
    target#draw ?blend_mode ?texture ?transform ?shader circle

  let draw_croix (target : #render_target) ?blend_mode ?texture ?transform ?shader depth pos0 dim0 fill_color =    
    let position = (fst pos0) +. (dim0/.4.), (snd pos0) +. (dim0/.4.) in 
    let carre = new rectangle_shape  
      ~size:(dim0/.2.,dim0/.2.) 
      ~position 
      ~fill_color
      ~outline_color:Color.black 
      ~outline_thickness:(min 2.0 (dim0/.200.)) ()
    in 
    target#draw ?blend_mode ?texture ?transform ?shader carre

  let draw_joueur target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 = function
    | Croix -> draw_croix target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 Color.blue
    | Cercle -> draw_cercle target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 Color.red

  let draw_joueur_tmp target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 = function
    | Croix -> draw_croix target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 
	(Color.rgb 160 160 255)
    | Cercle -> draw_cercle target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 
	(Color.rgb 255 160 160)
end

module DrawMorpion = DrawMorpion(GeometryParameters.DefaultParameters)(DrawFunctionImplementation)

let draw = DrawMorpion.draw_morpion
  
module Select = MorpionSelect.Select(GeometryParameters.DefaultParameters)

let rec access_at_path = function 
  | [], x -> x
  | hd :: tl, Grille m -> access_at_path (tl, access m hd)
  | _ -> assert false

let get_position_coup pos0 dim0 morpion pos = 
  let pos' = fst pos -. fst pos0, snd pos -. snd pos0 in 
  Select.select pos' dim0 morpion

let get_position_coup_vide pos0 dim0 morpion pos = 
  let pos' = fst pos -. fst pos0, snd pos -. snd pos0 in 
  let l = Select.select pos' dim0 morpion in
  match access_at_path (l, morpion) with 
    | Vide -> Some l
    | _ -> None 

let rec modify_at_path = function 
  | [a], Grille m -> modify m a
  | hd :: tl, Grille m -> modify_at_path (tl, access m hd)
  | _ -> assert false 

let rec get_geometry_at_path pos0 dim0 = function 
  | [] -> pos0, dim0
  | x::xs -> 
      let open GeometryParameters.DefaultParameters in 
      let (i,j) = coords_from_morpion_case x in
      let pos' = (fst pos0 +. position dim0 i, snd pos0 +. position dim0 j)  in 
      get_geometry_at_path pos' (dim0*.ratio) xs

let split_last l = 
  let open List in 
  let l0 = rev l in 
  (rev (tl l0), hd l0)

let remove_last l = fst (split_last l)

let victoire morpion l = 
  let grid_path, x = split_last l in 
  let m = match access_at_path (grid_path,morpion) with
    | Grille m -> m
    | _ -> assert false
  in 
  let cmp a b = 
    match (access m a), (access m b) with 
      | Coup j1, Coup j2 -> j1 = j2
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


let rec process_victoire joueur morpion l = 
  if victoire morpion l 
  then 
    if List.length l = 1 
    then true (* victoire globale *)
    else (* il reste des niveaux au dessus *)
      let l' = remove_last l in 
      modify_at_path (l',morpion) (fun _ -> Coup joueur) ;
      (* ATTENTION penser à redessiner *)
      process_victoire joueur morpion l' 
  else false


let path_for_rect pos0 dim0 morpion rect = 
  let c1 = get_position_coup pos0 dim0 morpion (rect.left, rect.top) in 
  let c2 = get_position_coup pos0 dim0 morpion (rect.left+.rect.width, rect.top) in 
  let c3 = get_position_coup pos0 dim0 morpion (rect.left, rect.top+.rect.height) in 
  let c4 = get_position_coup pos0 dim0 morpion (rect.left+.rect.width, rect.top+.rect.height) in 
  let rec common_factor = function 
    | x::xs, y::ys when x = y -> x::(common_factor (xs,ys))
    | _, _ -> []
  in 
  common_factor (c1, common_factor (c2, common_factor (c3,c4)))

let draw_at_path (target : #render_target) ?blend_mode ?texture ?transform ?shader depth pos dim morpion chemin =
  draw target ?blend_mode ?texture ?transform ?shader depth pos dim 
    (access_at_path (chemin, morpion)) ; 
  (8./.28.)**(float (List.length chemin))
  
  
let draw_with_rect  (target : #render_target) ?blend_mode ?texture ?transform ?shader pos0 dim0 morpion rect =
  let chemin_draw = path_for_rect pos0 dim0 morpion rect in
  let (pos, dim) = get_geometry_at_path pos0 dim0 chemin_draw in 
  Printf.printf "%f %f\n" (fst pos) (snd pos) ;
  draw target ?blend_mode ?texture ?transform ?shader ~rect (List.length chemin_draw) pos dim 
    (access_at_path (chemin_draw, morpion)) 

let print_case pos0 dim0 = function  
  | None -> Printf.printf "pas de coup\n"
  | Some l -> 
      let ((x,y), dim) = get_geometry_at_path pos0 dim0 l in   
      Printf.printf "%s => %f %f %f"  (String.concat ", " (List.map string_of_morpion_case l))
	x y dim
