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



let init_client (app : #render_window) = 
  let socket = new tcp_socket in
  let buf = Buffer.create 0 in 

  let enter_msg = new text ~string:"Connecting to server...\nEnter address and press Return\nEsc to exit" ~character_size:48 ~color:Color.black () in
  place_middle enter_msg 200. ;

  let input = new text ~character_size:48 ~color:Color.black () in

  let module M = struct type out = Quit | Connected | Continue end in 

  let try_connection b = 
    if socket#connect (new ip_address (`String (Buffer.contents buf))) connection_port = Done
    then M.Connected
    else b
  in 

  let process_event b evt =
    let open Event in 
    match evt with 
      | Closed -> app#close ; M.Quit 
      | KeyPressed { code = KeyCode.Escape ; _ } -> M.Quit 
      | KeyPressed { code = KeyCode.Return ; _ } -> try_connection b 
      | TextEntered { unicode } -> Buffer.add_char buf (Char.chr unicode) ; b
      | _ -> b
  in 
  
  let display () = 
    app#clear ~color:Color.white () ;
    app#draw enter_msg ;
    input#set_string (Buffer.contents buf) ;
    place_middle input 500. ;
    app#draw input ;
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
