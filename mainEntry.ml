open OcsfmlWindow
open OcsfmlGraphics
open Camera2d
open Morpion
open Common

let main_entry (app : #render_window) = 
  let name = new text ~string:"Morpion Normal" ~style:[Bold] ~character_size:72 ~color:Color.green () in 
  let begin_debug = new text ~string:"Debug" ~character_size:64 ~color:Color.yellow () in
  let two_players = new text ~string:"2 players" ~character_size:64 ~color:Color.yellow () in
  let host = new text ~string:"Host" ~character_size:64 ~color:Color.yellow () in
  let client = new text ~string:"Client" ~character_size:64 ~color:Color.yellow () in

  let board = [ 
    begin_debug, DebugGame ; 
    two_players, TwoPlayers ;
    host, Host ;
    client, Client
  ] in 
  
  place_middle name 75. ;
  List.fold_left (fun y (txt, _) -> place_middle txt y; y+.150.) 400. board ;

  let display () = 
    app#clear ~color:Color.blue () ;
    app#draw name ;
    List.iter (fun (txt, _) -> app#draw txt) board ;
    app#display 
  in 
  
  let process_event b evt = 
    let open Event in 
    match evt with 
      | Closed -> app#close ; b 
      | KeyPressed { code = KeyCode.Escape ; _ } -> app#close ; b 
      | MouseButtonPressed (_, { x ; y }) -> 
	  let pos = app#convert_coords (x,y) in 
	  let select_rect b (txt, ret) = 
	    if FloatRect.contains_v txt#get_global_bounds pos
	    then Some ret
	    else b
	  in List.fold_left select_rect b board
      | _ -> b
  in 

  let rec event_loop () = 
    let b = while_opt (fun () -> app#poll_event) process_event None in 
    display () ;
    
    if app#is_open 
    then match b with 
      | None -> event_loop ()
      | x -> x
    else None
  in
  event_loop () 
