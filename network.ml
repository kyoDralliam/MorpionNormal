open OcsfmlGraphics
open OcsfmlWindow
open OcsfmlNetwork
open Common
open HostGame
open ClientGame

let connection_port = Port.from_int 42424

let init_host (app : #render_window) = 
  let listener = new tcp_listener in
  listener#set_blocking false ;
  listener#listen connection_port ;
  let waiting_msg = new text ~string:"Waiting for client...\nEsc to exit" ~character_size:48 ~color:Color.black () in
  place_middle waiting_msg 400. ;

  let get_connection () =  
    let client = new tcp_socket in
    if  listener#accept client = Done
    then Some client
    else None
  in 

  let process_event b evt =
    let open Event in 
    match evt with 
      | Closed -> app#close ; false 
      | KeyPressed { code = KeyCode.Escape ; _ } -> false 
      | _ -> b
  in 

  let display () = 
    app#clear ~color:Color.white () ;
    app#draw waiting_msg ;
    app#display 
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event true in 
    let connection = get_connection () in 
    display () ;
    match connection with
	None -> if app#is_open && b then event_loop () else Some (EntryPoint)
      | Some c -> main_one_player_host app c
  in 

  event_loop ()

open Input

let init_client (app : #render_window) = 
  let socket = new tcp_socket in

  let module M = struct type out = Quit | Connected | Continue end in 

  let enter_msg = new text ~string:"Connecting to server...\nEnter IP address and press Return\nEsc to exit" ~character_size:48 ~color:Color.black () in

 let input_ip = 
   let try_connection s = 
     if socket#connect (new ip_address (`String s)) connection_port = Done
     then Some M.Connected
     else None
   in 
   new input (200., 35.) try_connection in 

  place_middle enter_msg 200. ;
  input_ip#set_position 300. 400. ;

  let process_event b evt =
    let open Event in 
    match evt with 
      | Closed -> app#close ; M.Quit 
      | KeyPressed { code = KeyCode.Escape ; _ } -> M.Quit 
      | KeyPressed { code = KeyCode.Return ; _ } -> 
	  (match input_ip#return with Some x -> x | None -> b)
      | KeyPressed { code = KeyCode.Back ; _ } -> input_ip#suppr ; b
      | TextEntered { unicode } -> input_ip#add_text (Char.chr unicode) ; b
      | MouseButtonPressed (_, {x;y}) -> 
	  if input_ip#selected (app#convert_coords (x,y)) 
	  then input_ip#set_focus true 
	  else input_ip#set_focus false ; b
      | _ -> b
  in 
  
  let display () = 
    app#clear ~color:Color.white () ;
    app#draw enter_msg ;
    app#draw input_ip ;
    app#display 
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event M.Continue in 
    display () ;
    match b with
	M.Quit -> Some (EntryPoint) 
      | M.Continue -> event_loop ()
      | M.Connected -> main_one_player_client app socket
  in 

  event_loop ()
