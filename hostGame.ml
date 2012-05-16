open OcsfmlSystem
open OcsfmlWindow
open OcsfmlGraphics
open OcsfmlNetwork
open Camera2d
open Morpion
open Common



let main_one_player_host app (socket : #tcp_socket) = 
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 
  let ck = new clock in 
  let time_before_exit = new text ~character_size:64 ~color:Color.black () in 
  
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
      | MouseButtonPressed (_, { x ; y }) ->  b
      | _ -> b
  in 
  
  let display t = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    camera2D#disable ;
    time_before_exit#set_string (Printf.sprintf "%f seconds remains before exit..." t);
    place_middle time_before_exit 600. ;
    app#draw time_before_exit ;
    camera2D#enable ;
    app#display  
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    let t = 5. -. (Time.as_seconds ck#get_elapsed_time) in 
    if t >= 0. then (display t ; if app#is_open && b then event_loop ()) 
  in 
  
  event_loop () ;

  let pck = new packet in 
  pck << (`String "quit") ;
  socket#send_packet pck ;

  camera2D#disable ;
  Some (EntryPoint)
