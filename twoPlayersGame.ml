
open OcsfmlWindow
open OcsfmlGraphics
open Camera2d
open Morpion
open Common



let main_two_players app = 
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 
  
  let player = ref Croix in 
  let grid = ref false in 
  let gagnant = ref None in 

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

  let gagne = 
    let msg = new text ~style:[Bold] ~character_size:96 ~color:Color.red () in 
    let eval () = 
      match !gagnant with 
	| None -> ()
	| Some Croix -> (msg#set_string "Croix a gagné\nEsc pour quitter" ; place_middle msg 300. ; app#draw msg)
	| Some Cercle -> (msg#set_string "Cercle a gagné\nEsc pour quitter" ; place_middle msg 300. ; app#draw msg)
    in 
    eval
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
	  if !gagnant = None 
	  then 
	    let pos = app#convert_coords (x,y) in 
	    let path = get_position_coup_vide pos morpion in 
            begin match path with 
	      | None -> b
	      | Some l -> 
		  if !grid
		  then apply_morpion morpion l (fun geom _ -> creer_grille geom)
		  else ( 
		    apply_morpion morpion l (creer_joueur !player) ;
		    if process_victoire !player morpion l 
		    then gagnant := Some !player 
		    else player := (match !player with Croix -> Cercle | Cercle -> Croix) 
		  );  
		  b
	    end 
	  else b
      | _ -> b
  in 

  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    camera2D#disable ;
    cursor () ;
    gagne () ;
    camera2D#enable ;
    app#display 
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    display () ;
    if app#is_open && b then event_loop () 
  in 

  event_loop () ;
  camera2D#disable ;
  Some (EntryPoint)

