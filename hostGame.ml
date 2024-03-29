open OcsfmlWindow
open OcsfmlGraphics
open OcsfmlNetwork
open Camera2d
open Common
open MorpionDef
open Morpion
open HostClientCommon

let morpion_width = 500.
let morpion_pos = 150.0,50.0

class play = 
object 
  val mutable coup_host = None 
  val mutable coup_client = None 
  val mutable tps_client = false 
  val mutable tps_host = 0.0

  method finished = 
    tps_client && tps_host >= playing_time
  method set_coup (chemin : chemin) = function 
    | Host -> coup_host <- Some chemin
    | Client -> coup_client <- Some chemin
  method set_tps_client = tps_client <- true 
  method set_tps_host t = tps_host <- t
  method get_result = coup_host, coup_client
end



let main_one_player_host app (socket : #tcp_socket) = 
  let morpion = creer_grille () in 
  let camera2D = camera2D app (float app#get_width) (float app#get_height) in 
  socket#set_blocking false ;

  let ck = new my_clock in 
  
  let time_before_exit = new Jauge.jauge ~tmax:playing_time ~position:(50.,50.) ~bg_color:Color.blue (300., 50.) in 
  
  let sh = new shader ~fragment:(ShaderSource.file "grey.fs") () in

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
      | KeyPressed { code = KeyCode.Space ; _ } -> 
	  ignore (send `EnterPause socket) ; 
	  ck#pause ; Some Pause
      | _ -> None 
  in 
  
  let process_quit_pause_event b evt =
    let open Event in 
    match evt with
      | KeyPressed { code = KeyCode.Space ; _ } -> 
	  ignore (send `ResumePause socket) ; 
	  ck#resume ; Some Playing
      | _ -> None 
  in 

  let process_playing_event play b evt =
    let open Event in 
    match evt with
      | MouseButtonPressed (_, {x ; y}) -> 
	  let pos = app#convert_coords (x,y) in 
	  let path = get_position_coup_vide morpion_pos morpion_width morpion pos in 
          begin match path with 
	    | None -> ()
	    | Some l -> play#set_coup l Host ; 
		modify_at_path (l, morpion) (fun _ -> creer_joueur_tmp Croix)
	  end ;
	  Some b
      | _ -> None 
  in 
  
  let display () = 
    app#clear ~color:Color.white () ;
    draw app 0 morpion_pos morpion_width morpion ;
    camera2D#disable ;
    app#draw time_before_exit ;
    camera2D#enable ;
    app#display   
  in 

  let display_pause () = 
    app#clear ~color:(Color.rgb 178 178 178) () ;
    draw app ~shader:sh 0 morpion_pos morpion_width  morpion ;
    camera2D#disable ;
    app#draw ~shader:sh time_before_exit ;
    app#draw (new rectangle_shape ~size:(100.,100.) ~position:(100.,100.) ~fill_color:Color.red ~outline_color:Color.black ~outline_thickness:2.0 ()) ;
    camera2D#enable ;
    app#display  
  in 

  let display_finished j = 
    display ()
  in 

  let process_remote_messages play state = 
    match receive socket with 
      | Some (`Played chemin) -> 
	  play#set_coup chemin Client ; 
	  state
      | Some `EndTimer -> play#set_tps_client ; state 
      | Some `EnterPause -> ck#pause ; Pause
      | Some `ResumePause -> ck#resume ; Playing
      | Some `Quit -> Exit
      | None -> state
  in 

  let process_result play = 
      match play#get_result with 
      | Some l1, Some l2 when l1 = l2 -> 
	  send (`AddGrid l1) socket ;
	  modify_at_path (l1, morpion) (fun _ -> creer_grille ()) 
      | (x, y) as chemins -> 
	  send (`AddNormal chemins) socket ; 
	  (match x with 
	     | Some l1 -> modify_at_path (l1, morpion) (fun _ -> creer_joueur Croix) 
	     | None -> ()) ;
	  (match y with 
	     | Some l2 -> modify_at_path (l2, morpion) (fun _ -> creer_joueur Cercle) 
	     | None -> ())
  in 

  let rec main_loop state play = 
    if play#finished
    then
      begin 
	process_result play ;
	ignore ck#restart ; 
	main_loop state (new play)
      end 
    else 
      begin 
	let state = process_remote_messages play state in 
	match state with 
	  | Playing -> 
	      let state = while_opt (fun () -> app#poll_event) 
		(process_event [
		   process_quit_event ; 
		   process_camera_event ; 
		   process_playing_event play ; 
		   process_enter_pause_event])
		Playing in 
	      play#set_tps_host ck#get_time ;
	      time_before_exit#update ck#get_time ;
	      camera2D#update ;
	      display () ; 
	      if app#is_open then main_loop state play 
	      else send `Quit socket
	  | Pause -> 
	      let state = while_opt (fun () -> app#poll_event) (process_event [process_quit_event ; process_quit_pause_event]) Pause in 
	      display_pause () ;
	      if app#is_open then main_loop state play 
	      else send `Quit socket
	  | Finished j -> 
	      let state = while_opt (fun () -> app#poll_event) (process_event [process_quit_event]) (Finished j) in 
	      display_finished j ;
	      if app#is_open then main_loop state play 
	      else send `Quit socket
	  | Exit -> send `Quit socket
      end 
  in 
  
  ignore (main_loop Playing (new play)) ;

  camera2D#disable ;
  Some (EntryPoint)
