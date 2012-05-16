open OcsfmlGraphics

let place_middle txt y = 
  let rect = txt#get_global_bounds in 
  txt#set_position ((800. -. rect.width)/.2.) ((y -. rect.height)/.2.)

let rec while_opt f g x = match f () with 
  | Some y -> while_opt f g (g x y)
  | None -> x

type game = 
    EntryPoint (* main_entry_point *)
  | DebugGame (* main_morpion_debug *)
  | TwoPlayers (* main_two_players *)
  | Host (* *)
  | Client
