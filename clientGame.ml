open OcsfmlWindow
open OcsfmlGraphics
open OcsfmlNetwork
open Camera2d
open Morpion
open Common


let main_one_player_client app (socket : #tcp_socket) =  
  let morpion = creer_grille { position = 150.,50. ; dimension = 500. } in 
  let camera2D = camera2D app in 

  socket#set_blocking false ;

  let waiting_host = new text ~string:"Waiting for an answer of the host" ~character_size:64 ~color:Color.black () in 
  place_middle waiting_host 600. ;

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
  
  let display () = 
    app#clear ~color:Color.white () ;
    draw app morpion ;
    camera2D#disable ;
    app#draw waiting_host ;
    camera2D#enable ;
    app#display  
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    camera2D#update ;
    display () ;
    let pck = new packet in 
    match socket#receive_packet pck with 
      | Done -> if pck#read_string <> "quit" then event_loop ()
      | NotReady -> if app#is_open && b then event_loop ()
      | _ -> print_endline "error or disconnected"
  in 


  event_loop () ;
  camera2D#disable ;
  Some (EntryPoint)
