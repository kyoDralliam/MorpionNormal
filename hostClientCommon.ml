open OcsfmlSystem
open OcsfmlNetwork
open Morpion

type pause_event = [ `EnterPause | `ResumePause | `Quit ]
type client_sent_event = [ `Played of chemin | `EndTimer | pause_event ]
type host_sent_event = [`AddGrid of chemin | `AddNormal of chemin option * chemin option | pause_event ]

type computer = Client | Host 

let playing_time = 10.0
let waiting_time = 3.0

class my_clock = 
object (self)
  inherit clock as super
  val mutable time = 0.
  method private update = time <- time +. (Time.as_seconds super#restart)
  method pause = self#update
  method resume = ignore super#restart
  method get_time = self#update ; time
  method restart = time <- 0. ; super#restart
end

type state = Playing | Pause | Finished of joueur | Exit

let send x s = 
  s#send_string (Marshal.to_string x [])

let receive (type t) =
  let buf = String.create 1024 in
  fun (s : tcp_socket) -> 
    let status, i = s#receive_string buf in 
    if status = Done 
    then Some (Marshal.from_string (String.sub buf 0 i) 0 : t) 
    else None

let receive_host : #tcp_socket -> client_sent_event option = receive
  
let receive_client : #tcp_socket -> host_sent_event option = receive
