open OcsfmlSystem
open OcsfmlWindow
open OcsfmlGraphics

let camera2D (app : #render_target) = 
object (self) 
  val clock = new clock 
  val mutable vertical_mvt = 0
  val mutable horizontal_mvt = 0
  val view = new view (`Rect {top = 0. ; left = 0. ; width = 800. ; height = 600. })
  val mutable zoom_factor = 1.

  method private activate b1 b2 = function 
    | (1 | -1) as i -> i
    | 0 -> 
	begin match b1, b2 with 
	| true, true -> assert false 
	| true, false -> -1 
	| false, true -> 1
	| false, false -> 0
	end
    | _ -> assert false

  method activate_horizontal i = 
    horizontal_mvt <- self#activate 
      (Keyboard.is_key_pressed KeyCode.Left) 
      (Keyboard.is_key_pressed KeyCode.Right) i

  method activate_vertical i = 
    vertical_mvt <- self#activate 
      (Keyboard.is_key_pressed KeyCode.Down)
      (Keyboard.is_key_pressed KeyCode.Up) i

  method update = 
    let t = (Time.as_seconds clock#restart)*.300.*. zoom_factor in
    if Keyboard.is_key_pressed KeyCode.Left || Keyboard.is_key_pressed KeyCode.Right
    then view#move ((float horizontal_mvt)*.t) 0. ;
    if Keyboard.is_key_pressed KeyCode.Up || Keyboard.is_key_pressed KeyCode.Down
    then view#move 0. ((float vertical_mvt)*.t) ;
    app#set_view view

  method scroll k x y =
    let (x',y') = app#convert_coords (x,y) in 
    let (cx, cy) = view#get_center in 
    let z = exp ((float k)/.10.) in 
    let goal_x = (1.-.z)*.(x'-.cx) and goal_y = (1.-.z)*.(y'-.cy) in
    zoom_factor <- zoom_factor *. z ;
    view#zoom z ;
    view#move goal_x goal_y

  method recenter geometry = 
    let open Morpion in 
    view#set_size (geometry.dimension *. (float app#get_width) /. (float app#get_height)) geometry.dimension ;
    view#set_center (fst geometry.position +. (geometry.dimension/.2.)) (snd geometry.position +. (geometry.dimension/.2.)) ;
    view#zoom 1.15 ;
    app#set_view view
(*
    let rect = { 
      left = fst geometry.position ; 
      top = snd geometry.position ; 
      width = geometry.dimension *. (float app#get_width) /. (float app#get_height) ; 
      height = geometry.dimension  
    } 
    in 
    view#reset rect *)

  method get_view = view
end
