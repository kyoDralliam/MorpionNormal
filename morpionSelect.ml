open MorpionDef
open MorpionDraw

module Select(GP : GeometryParameters) =
struct

  open GP

  let antipos x dim =
    let ik = int_of_float (x *. (float total_width) /. dim) in 
    if 0 <= ik && ik < 28
    then 
      [| -1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 
	 -1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 
	 -1 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; -1 |].(ik) 
    else -1
      
  let rec select pos dim0 = function 
    | Grille m -> 
	let x = antipos (fst pos) dim0 in 
	let y = antipos (snd pos) dim0 in 
	if x < 0 || y < 0 
	then []
	else 
	  let pos' = (fst pos -. position dim0 x, snd pos -. position dim0 y) in 
	  (morpion_case_from_coords x y)::(select pos' (dim0*.ratio) m.(x).(y)) 
    | Vide -> []
    | Coup _ -> [] 
    | CoupTmp _ -> []
	
end
