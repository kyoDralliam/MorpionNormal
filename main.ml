open OcsfmlWindow
open OcsfmlGraphics
open Camera2d
open Morpion


let rec while_opt f g x = match f () with 
  | Some y -> while_opt f g (g x y)
  | None -> x

let main_morpion_debug app =
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 
  
  let cursor = 
    let cercle_spr = new circle_shape ~radius:30. ~fill_color:Color.red ~outline_color:Color.black ~outline_thickness:2. () in
    let croix_spr = new rectangle_shape ~size:(40.,40.) ~fill_color:Color.blue ~outline_color:Color.black ~outline_thickness:2. () in 
    let grid_texture = new texture (`File "grid.png") in 
    let grid_spr = new sprite ~texture:grid_texture () in 
    cercle_spr # scale 0.75 0.75 ; croix_spr # scale 0.75 0.75 ; grid_spr # scale 0.75 0.75 ; 
    let eval () = 
      let x, y = app#convert_coords (Mouse.get_relative_position app) in 
      let pos = x +. 10.0, y +. 10.0 in 
      if Keyboard.is_key_pressed KeyCode.LControl 
      then (cercle_spr#set_position_v pos ; app#draw cercle_spr)
      else if Keyboard.is_key_pressed KeyCode.LAlt
      then (croix_spr#set_position_v pos ; app#draw croix_spr)
      else if Keyboard.is_key_pressed KeyCode.LShift
      then (grid_spr#set_position_v pos ; app#draw grid_spr)
    in eval
  in 

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
		else if Keyboard.is_key_pressed KeyCode.Space
		then camera2D#recenter (get_geometry morpion l)
		else print_position_coup (Some l) ;
		b end
      | _ -> b
  in 

  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    app#set_view app#get_default_view ;
    cursor () ;
    app#set_view camera2D#get_view ; 
    app#display 
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    display () ;
    if app#is_open && b then event_loop () 
  in 

  event_loop ()

let main_two_players app = 
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 
  
  let player = ref Croix in 
  let grid = ref false in 

  let cursor = 
    let cercle_spr = new circle_shape ~radius:30. ~fill_color:Color.red ~outline_color:Color.black ~outline_thickness:2. () in
    let croix_spr = new rectangle_shape ~size:(40.,40.) ~fill_color:Color.blue ~outline_color:Color.black ~outline_thickness:2. () in 
    let grid_texture = new texture (`File "grid.png") in 
    let grid_spr = new sprite ~texture:grid_texture () in 
    cercle_spr # scale 0.75 0.75 ; croix_spr # scale 0.75 0.75 ; grid_spr # scale 0.75 0.75 ; 
    let eval () = 
      let x, y = app#convert_coords (Mouse.get_relative_position app) in 
      let pos = x +. 10.0, y +. 10.0 in 
      if !grid  
      then (grid_spr#set_position_v pos ; app#draw grid_spr)
      else 
	match !player with 
	  | Croix -> (croix_spr#set_position_v pos ; app#draw croix_spr)
	  | Cercle -> (cercle_spr#set_position_v pos ; app#draw cercle_spr)
    in eval
  in 

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
      | KeyReleased { code = KeyCode.Space ; _ } -> grid := not !grid ; b
      | MouseWheelMoved ( k , { x ; y } ) -> camera2D#scroll k x y ; b
      | MouseButtonPressed (_, { x ; y }) -> 
	  let pos = app#convert_coords (x,y) in 
	  let path = get_position_coup_vide pos morpion in 
          begin match path with 
	    | None -> b
	    | Some l -> 
		if !grid
		then apply_morpion morpion l (fun geom _ -> creer_grille geom)
		else ( 
		  apply_morpion morpion l (creer_joueur !player) ;
		  if process_victoire !player morpion l then print_endline "un joueur a gagné" ;
		  player := (match !player with Croix -> Cercle | Cercle -> Croix) 
		);  
		b
	  end
      | _ -> b
  in 

  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    app#set_view app#get_default_view ;
    cursor () ;
    app#set_view camera2D#get_view ; 
    app#display 
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    display () ;
    if app#is_open && b then event_loop () 
  in 

  event_loop ()







let main_entry (app : #render_window) = 
  let name = new text ~string:"Morpion Normal" ~style:[Bold] ~character_size:96 ~color:Color.green () in 
  let begin_debug = new text ~string:"Debug" ~character_size:96 ~color:Color.yellow () in
  let two_players = new text ~string:"2 players" ~character_size:96 ~color:Color.yellow () in

  let place_middle txt y = 
    let rect = txt#get_global_bounds in 
    txt#set_position ((800. -. rect.width)/.2.) ((y -. rect.height)/.2.)
  in 
  
  place_middle name 75. ;
  place_middle begin_debug 400. ;
  place_middle two_players 550. ;

  let display () = 
    app#clear ~color:Color.blue () ;
    app#draw name ;
    app#draw begin_debug ;
    app#draw two_players ;
    app#display 
  in 
  
  let process_event b evt = 
    let open Event in 
    match evt with 
      | Closed -> app#close ; b 
      | KeyPressed { code = KeyCode.Escape ; _ } -> app#close ; b 
      | MouseButtonPressed (_, { x ; y }) -> 
	  let pos = app#convert_coords (x,y) in 
	  if FloatRect.contains_v begin_debug#get_global_bounds pos
	  then Some (fun () -> main_morpion_debug app)
	  else if FloatRect.contains_v two_players#get_global_bounds pos
	  then Some (fun () -> main_two_players app)
	  else b
      | _ -> b
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event None in 
    display () ;
    
    if app#is_open 
    then match b with 
      | None -> event_loop ()
      | Some f -> f () ; event_loop ()
  in
  event_loop ()

let _ = 
  let app = new render_window (VideoMode.create ()) "morpion" in
  app#set_framerate_limit 60 ;
  main_entry app 
 
