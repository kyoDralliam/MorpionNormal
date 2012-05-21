open OcsfmlGraphics


let auto_character_size ?style size = 
  let test = new text ~string:"Lj" ?style () in 
  let rec get_max x = 
    test#set_character_size x ;
    if test#get_local_bounds.height <= 0.8 *. (snd size)
    then get_max (2*x)
    else x
  in 
  let rec get_correct x delta = 
    if delta <= 1 then x
    else 
      begin 
	test#set_character_size x ;
	if test#get_local_bounds.height <= 0.8 *. (snd size)
	then get_correct (x+delta) (delta/2)
	else get_correct (x-delta) (delta/2)
      end 
  in 
  let x0 = get_max 16 in 
  get_correct x0 (x0/2)


class ['a] input size ?style 
  ?(character_size = auto_character_size ?style size) 
  ?(fill_color = Color.white) 
  ?(outline_thickness = 2.5) 
  ?(outline_color = Color.black)  
  ?position 
  ?scale
  ?origin
  ?rotation
  (on_return : string -> 'a option) = 
object (self)
  inherit transformable ?position ?scale ?origin ?rotation ()
  inherit drawable ~overloaded:`draw (Drawable.inherits ())
  val buf = Buffer.create 16
  val input_zone = new rectangle_shape ~size ~fill_color ~outline_thickness ~outline_color ()
  val input_text = new text ~color:Color.black ~character_size ?style () 
  val mutable focus = false
    
  method set_focus b = 
    focus <- b ; 
    if focus 
    then input_zone#set_outline_color Color.cyan
    else input_zone#set_outline_color outline_color

  method has_focus = focus

  method add_text c = if focus then Buffer.add_char buf c

  method suppr = 
    if focus 
    then 
      let s = Buffer.contents buf in 
      Buffer.clear buf ; 
      Buffer.add_substring buf s 0 ((String.length s)-1)

  method return = if focus then on_return (Buffer.contents buf) else None

   method draw app blend_mode transform0 texture shader = 
      let transform = self#get_transform#combine transform0 in 
      app#draw 
	~blend_mode
	~transform
	~texture ~shader input_zone ;
      input_text#set_string (Buffer.contents buf) ;
      app#draw 
	~blend_mode
	~transform
	~texture ~shader input_text 

  method selected (x,y) =  
    let (x,y) = self#get_inverse_transform#transform_point x y in 
    FloatRect.contains input_zone#get_global_bounds x y
end 


