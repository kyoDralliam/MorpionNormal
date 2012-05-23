open OcsfmlGraphics
open MorpionDef

module type GeometryParameters =
sig

  val bordure_width : int
  val case_width : int
  val total_width : int
  val ratio : float

  val max_depth : int

  val position : float -> int -> float
end

module GeometryParameters = 
struct 

  module type GeometryParametersOptions = 
  sig 
    val bordure_width : int
    val case_width : int

    val max_depth : int
  end 

  module Make(GPO : GeometryParametersOptions) =
  (struct 
    include GPO 
    let total_width = 4*bordure_width + 3*case_width
    let ratio = float case_width /. float total_width
    let position dim0 i = 
      float ((i+1) * bordure_width + i*case_width) *. dim0 /. (float total_width)
   end : GeometryParameters)

  module DefaultParameters = 
    Make( 
      struct 
	let bordure_width = 1
	let case_width = 8
	let max_depth = 6
      end
    )
end

type ('a, 'b) params = 
    (#render_target as 'b) -> 
      ?blend_mode:blend_mode -> 
  ?texture:texture -> 
  ?transform:transform -> 
  ?shader:shader -> int -> 
  float * float -> float -> 'a

module type DrawFunction = 
sig 
  val draw_grid : (unit, 'a) params
  val draw_vide : (unit, 'a) params
  val draw_joueur : (joueur -> unit, 'a) params
  val draw_joueur_tmp : (joueur -> unit, 'a) params
end

let rect_from_pos (left,top) (width,height) = {left ; top ; width ; height} 
  

module DrawMorpion(GP : GeometryParameters)(DF : DrawFunction) =
struct
  open GP
  open DF

  let rec draw_morpion target ?blend_mode ?texture ?transform ?shader ?rect depth pos0 dim0 morpion = 
    if match rect with 
      | Some r -> (FloatRect.intersects r (rect_from_pos pos0 (dim0,dim0))) <> None
      | None -> true
    then
      match morpion with 
	| Grille m -> 
	    draw_grid target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 ; 
	    let subdraw i j c = 
	      draw_morpion ?blend_mode ?texture ?transform ?shader target depth 
		(fst pos0 +. position dim0 i, snd pos0 +. position dim0 j) 
		(dim0 *. ratio) c in 
	    Array.iteri (fun i v -> Array.iteri (subdraw i) v) m
	| Coup joueur -> 
	    draw_vide target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 ;
	    draw_joueur target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 joueur
	| CoupTmp joueur ->
	    draw_vide target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 ;
	    draw_joueur target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 joueur ;
	    draw_joueur_tmp target ?blend_mode ?texture ?transform ?shader depth pos0 dim0 joueur
	| Vide -> draw_vide target ?blend_mode ?texture ?transform ?shader depth pos0 dim0
end
