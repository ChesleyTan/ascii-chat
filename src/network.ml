open Package
open Utils
open Lwt

let my_address = Utils.get_address_self()

let my_inet_address = Unix.inet_addr_of_string my_address

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr)
                                   ^ ":" ^ (string_of_int port)

let connections = Hashtbl.create 4

let send package =
  failwith "Unimplemented"

let broadcast_gossip () =
  let gossip = Hashtbl.fold (fun k _ s -> s ^ ";" ^ k) connections "G" in
  let send_gossip _ oc = output_string oc gossip in
  Hashtbl.iter send_gossip connections;
  print_endline @@ "Broadcasting gossip: " ^ gossip

let rec update_connection cb c =
  if Hashtbl.mem connections c then ()
  else
    match Str.split (Str.regexp ":") c with
    | [addr; port] ->
        begin
          print_endline @@ "Connecting to " ^ addr ^":"^port;
          let open Unix in
          let sock = socket PF_INET SOCK_STREAM 0 in
          setsockopt sock SO_REUSEADDR true;
          let remote_addr = ADDR_INET (Unix.inet_addr_of_string addr,
                                       int_of_string port) in
          connect sock remote_addr;
          accept_connection cb false (sock, remote_addr)
        end
    | _ -> ()

and handle_gossip cb gossip =
  let conns = Str.split (Str.regexp ";") gossip in
    List.iter (update_connection cb) conns

and handle_msg cb id ic msg_len =
  match safe_int_of_string msg_len with
  | Some len ->
      if len > 0 then
        let ciphertext = really_input_string ic len in
        print_endline @@ "Received: " ^ ciphertext;
        let package = ciphertext |> decrypt |> deserialize in
          cb id package
      else print_endline @@ "Message length cannot be 0"
  | None -> print_endline @@ "Message length not an integer: " ^ msg_len

and handle_line cb id ic line =
  if String.length line > 0 then
    let data = String.sub line 1 (String.length line - 1) in
    match String.get line 0 with
    | 'G' -> handle_gossip cb data
    | 'M' -> handle_msg cb id ic data
    | c -> print_endline @@ "Received unknown header: " ^ (String.make 1 c)
  else print_endline @@ "Received invalid line: " ^ line

and handle_connection cb id ic () =
  try
    let line = input_line ic in
    handle_line cb id ic line
  with
  | End_of_file -> () (* TODO: close connection *)

and accept_connection cb new_client (fd, remote_addr) =
  print_endline "new connection";
  let id = string_of_sockaddr remote_addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  Hashtbl.add connections id oc;
  if new_client then
    broadcast_gossip ()
  ;
  Lwt_preemptive.detach (fun () ->
      handle_connection cb id ic ()
  ) () |> ignore

(* Adapted from: http://baturin.org/code/lwt-counter-server/ *)
let network_initialize port cb =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let open Unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (my_inet_address, port);
  listen sock 4;
  let rec serve () = accept sock |> (accept_connection cb true) |> serve in
  serve()
