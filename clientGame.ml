open OcsfmlWindow
open OcsfmlGraphics
open OcsfmlNetwork
open Camera2d
open Morpion
open Common
open HostClientCommon



let main_one_player_client app (socket : #tcp_socket) =  
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 
  
  let ck = new my_clock in 
  let time_before_exit = new Jauge.jauge ~tmax:playing_time ~position:(50.,50.) ~bg_color:Color.blue (300., 50.) in 

  socket#set_blocking false ;

  let waiting_host = new text ~string:"Waiting for an answer of the host" ~character_size:64 ~color:Color.black () in 
  place_middle waiting_host 600. ;

  let process_quit_event b evt = 
    let open Event in 
    match evt with 
      | Closed -> app#close ; Some Exit
      | KeyPressed { code = KeyCode.Escape ; _ } -> Some Exit 
      | _ -> None
  in 

  let process_camera_event b evt = 
    let open Event in 
    match evt with
      | KeyPressed { code = KeyCode.Left ; _ } -> camera2D#activate_horizontal (-1) ; Some b
      | KeyPressed { code = KeyCode.Right ; _ } -> camera2D#activate_horizontal 1 ; Some b
      | KeyPressed { code = KeyCode.Up ; _ } -> camera2D#activate_vertical (-1) ; Some b
      | KeyPressed { code = KeyCode.Down ; _ } ->  camera2D#activate_vertical 1 ; Some b
      | KeyReleased { code = KeyCode.Left ; _ } -> camera2D#activate_horizontal 0 ; Some b
      | KeyReleased { code = KeyCode.Right ; _ } -> camera2D#activate_horizontal 0 ; Some b
      | KeyReleased { code = KeyCode.Up ; _ } -> camera2D#activate_vertical 0 ; Some b
      | KeyReleased { code = KeyCode.Down ; _ } ->  camera2D#activate_vertical 0 ; Some b
      | MouseWheelMoved ( k , { x ; y } ) -> camera2D#scroll k x y ; Some b
      | _ -> None 
  in 

  let process_enter_pause_event b evt =
    let open Event in 
    match evt with
      | KeyPressed { code = KeyCode.Space ; _ } -> ignore (send `EnterPause socket) ; Some Pause
      | _ -> None 
  in 
  
  let process_quit_pause_event b evt =
    let open Event in 
    match evt with
      | KeyPressed { code = KeyCode.Space ; _ } -> ignore (send `ResumePause socket) ; Some Playing
      | _ -> None 
  in 

  let process_playing_event b evt =
    let open Event in 
    match evt with
      | MouseButtonPressed (_, {x ; y}) -> Some b
      | _ -> None 
  in 
  
  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    camera2D#disable ;
    time_before_exit#draw app ;
    camera2D#enable ;
    app#display  
  in 

 let process_remote_messages state = 
    match receive socket with 
      | Some (`AddGrid chemin) -> ignore ck#restart ; state
      | Some (`AddNormal (cheminCroix, cheminCercle)) -> ignore ck#restart ; state 
      | Some `EnterPause -> ck#pause ; Pause
      | Some `ResumePause -> ck#resume ; Playing
      | Some `Quit -> Exit
      | None -> state
  in 

  let display_pause () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    camera2D#disable ;
    app#draw (new rectangle_shape ~size:(100.,100.) ~position:(100.,100.) ~fill_color:Color.red ~outline_color:Color.black ~outline_thickness:2.0 ()) ;
    camera2D#enable ;
    app#display  
  in 

  let display_finished j = 
    display ()
  in 

  let rec main_loop state = 
    let state = process_remote_messages state in 
    if ck#get_time >= playing_time
    then (ignore (send `EndTimer socket) ; ignore ck#restart) ;
    match state with 
      | Playing -> 
	  let state = while_opt (fun () -> app#poll_event) 
	    (process_event [
	       process_quit_event ; 
	       process_camera_event ; 
	       process_playing_event ; 
	       process_enter_pause_event])
	    Playing in 
	  time_before_exit#update ck#get_time ;
	  camera2D#update ;
	  display () ; 
	  if app#is_open then main_loop state 
	  else send `Quit socket
      | Pause -> 
	  let state = while_opt (fun () -> app#poll_event) (process_event [process_quit_event ; process_quit_pause_event]) Pause in 
	  display_pause () ;
	  if app#is_open then main_loop state 
	  else send `Quit socket
      | Finished j -> 
	  let state = while_opt (fun () -> app#poll_event) (process_event [process_quit_event]) (Finished j) in 
	  display_finished j ;
	  if app#is_open then main_loop state 
	  else send `Quit socket
      | Exit -> send `Quit socket
  in 

  ignore (main_loop Playing) ;
  camera2D#disable ;
  Some (EntryPoint)
