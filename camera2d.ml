open OcsfmlSystem
open OcsfmlWindow
open OcsfmlGraphics

let rect_from_center_size (x,y) (w,h) = {  left = x -. (w/.2.) ; top = y -. (h/.2.) ; width = w ; height = h } 

let camera2D (app : #render_target) w h = 
object (self) 
  val clock = new clock 
  val mutable vertical_mvt = 0
  val mutable horizontal_mvt = 0
  val view = new view 
    (`Rect {top = 0. ; left = 0. ; width = w ; height = h })
  val mutable zoom_modified = false 

  method private get_zoom = (fst view#get_size) /. w

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
    let t = (Time.as_seconds clock#restart)*.300.*. self#get_zoom in
    let b1 = Keyboard.is_key_pressed KeyCode.Left || Keyboard.is_key_pressed KeyCode.Right in
    if b1 then view#move ((float horizontal_mvt)*.t) 0. ;
    let b2 = Keyboard.is_key_pressed KeyCode.Up || Keyboard.is_key_pressed KeyCode.Down in
    if b2 then view#move 0. ((float vertical_mvt)*.t) ;
    app#set_view view ; 
    let z0 = zoom_modified in 
    zoom_modified <- false ;
    b1 || b2 || z0

  method scroll k x y =
    let (x',y') = app#convert_coords (x,y) in 
    let (cx, cy) = view#get_center in 
    let z = exp ((float k)/.10.) in 
    let goal_x = (1.-.z)*.(x'-.cx) and goal_y = (1.-.z)*.(y'-.cy) in
    zoom_modified <- true ;
    view#zoom z ;
    view#move goal_x goal_y

  method recenter (position, dimension) = 
    let open Morpion in 
    view#set_size (dimension *. w /. h) dimension ;
    view#set_center (fst position +. (dimension/.2.)) (snd position +. (dimension/.2.)) ;
    zoom_modified <- true ;
    view#zoom 1.15 ;
    app#set_view view

  method enable = app#set_view view

  method disable = app#set_view app#get_default_view

  method get_visual_rect = rect_from_center_size view#get_center view#get_size

end
