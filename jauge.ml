open OcsfmlGraphics

class jauge ?(t0=0.) ?(tmax=1.) ?(bg_color=Color.white) ?(begin_color=Color.yellow) ?(end_color=Color.red) ?position ?rotation ?scale ?origin size = 
  let t0 = max (min (t0/.tmax) 1.) 0. in 
  let addv (x,y) x' y' = (x+.x', y+.y') in
  let h = snd size in 
  let w t = t *. (fst size) in 
  let interpolation col1 col2 t = 
    Color.({ 
      r = int_of_float (t*.(float col2.r) +. (1.-.t)*.(float col1.r)) ; 
      g = int_of_float (t*.(float col2.g) +. (1.-.t)*.(float col1.g)) ; 
      b = int_of_float (t*.(float col2.b) +. (1.-.t)*.(float col1.b)) ; 
      a = 255
    }) in 
  let content0 = [
    mk_vertex ~position:(0.,0.) ~color:begin_color () ;
    mk_vertex ~position:(0., h) ~color:begin_color () ;
    mk_vertex ~position:(w t0, h) ~color:(interpolation begin_color end_color t0) () ;
    mk_vertex ~position:(w t0, 0.) ~color:(interpolation begin_color end_color t0) ()
  ]
  in 
object (self)

  inherit transformable ?position ?rotation ?scale ?origin () 
  inherit drawable ~overloaded:`draw (Drawable.inherits ())

  val content = new vertex_array ~primitive_type:Quads content0 
  val cadre = new rectangle_shape ~size ~fill_color:bg_color ~outline_color:Color.black ~outline_thickness:2.0 ()
  val mutable tmax = tmax

  method update t = 
    let t = min (t/.tmax) 1. in
    let pos = (content#get_at_index 0).position in
    content#set_at_index 2 
      (mk_vertex 
	 ~position:(addv pos (w t) h) 
	 ~color:(interpolation begin_color end_color t) ()) ;
    content#set_at_index 3
      (mk_vertex 
	 ~position:(addv pos (w t) 0.) 
	 ~color:(interpolation begin_color end_color t) ()) 
      
   method draw app blend_mode transform0 texture shader = 
      let transform = self#get_transform#combine transform0 in 
      app#draw 
	~blend_mode
	~transform
	~texture ~shader cadre ;
      app#draw 
	~blend_mode
	~transform
	~texture ~shader content 

  method set_max_val maxval = tmax <- maxval ; self#update 0.0

end
