open OcsfmlWindow
open OcsfmlGraphics
open Camera2d
open Morpion
open Common


let global_loop app = 
  let rec aux = function 
    | Some EntryPoint -> aux (MainEntry.main_entry app)
    | Some DebugGame -> aux (DebugGame.main_morpion_debug app)
    | Some TwoPlayers -> aux (TwoPlayersGame.main_two_players app)
    | Some Host -> aux (Network.init_host app)
    | Some Client -> aux (Network.init_client app)
    | None -> ()
  in 
  aux (Some EntryPoint)

let _ = 
  let app = new render_window (VideoMode.create ()) "morpion" in
  app#set_framerate_limit 60 ;
  global_loop app
 
