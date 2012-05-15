open OcsfmlWindow
open OcsfmlGraphics
open Camera2d
open Morpion2


let rec while_opt f g x = match f () with 
  | Some y -> while_opt f g (g x y)
  | None -> x


let _ = 
  let app = new render_window (VideoMode.create ()) "morpion" in
  app#set_framerate_limit 60 ;
  (*let renderer =
    try new render_texture 4096 4096 
    with CreateFailure -> failwith "Could not create the render texture"
  in
  renderer#clear ~color:Color.white () ;*)
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  apply_morpion morpion [Center] (fun geom _ -> creer_grille geom) ;
  apply_morpion morpion [NorthWest] (fun geom _ -> creer_grille geom) ;
  let camera2D = camera2D app in 
  
  let process_event b evt =
    let open Event in 
    match evt with 
      | Closed -> app#close ; false 
      | KeyPressed { code = KeyCode.Escape ; _ } -> false 
      | KeyPressed { code = KeyCode.Left ; _ } -> camera2D#activate_horizontal (-1) ; b
      | KeyPressed { code = KeyCode.Right ; _ } -> camera2D#activate_horizontal 1 ; b
      | KeyPressed { code = KeyCode.Up ; _ } -> camera2D#activate_vertical (-1) ; b
      | KeyPressed { code = KeyCode.Down ; _ } ->  camera2D#activate_vertical 1 ; b
      | KeyReleased { code = KeyCode.Left ; _ } -> camera2D#activate_horizontal 0 ; b
      | KeyReleased { code = KeyCode.Right ; _ } -> camera2D#activate_horizontal 0 ; b
      | KeyReleased { code = KeyCode.Up ; _ } -> camera2D#activate_vertical 0 ; b
      | KeyReleased { code = KeyCode.Down ; _ } ->  camera2D#activate_vertical 0 ; b
      | MouseWheelMoved ( k , { x ; y } ) -> camera2D#scroll k x y ; b
      | MouseButtonPressed (_, { x ; y }) -> 
	  let pos = app#convert_coords (x,y) in 
	  let path = get_position_coup pos morpion in 
          begin match path with 
	    | None -> b
	    | Some l -> 
		if Keyboard.is_key_pressed KeyCode.LControl 
		then apply_morpion morpion l (creer_joueur Cercle) 
		else if Keyboard.is_key_pressed KeyCode.LAlt
		then apply_morpion morpion l (creer_joueur Croix) 
		else if Keyboard.is_key_pressed KeyCode.LShift
		then apply_morpion morpion l (fun geom _ -> creer_grille geom)
		else print_position_coup (Some l) ;
		b end
      | _ -> b
  in 

  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    app#display
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    display () ;
    if app#is_open && b then event_loop () 
  in 

  event_loop ()
