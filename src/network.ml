open Package
open Utils
open Lwt

let print_debug_endline = if false then print_endline else ignore

let my_address = Utils.get_address_self()
let my_inet_address = Unix.inet_addr_of_string my_address
let my_id = ref ""

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr)
                                   ^ ":" ^ (string_of_int port)

let connections:((string, (in_channel * out_channel * Unix.file_descr)) Hashtbl.t)
  = Hashtbl.create 4

let broadcast data =
  let send_data id (_, oc, _) = output_string oc data; flush oc in
  Hashtbl.iter send_data connections

let send package =
  let msg = package |> serialize |> encrypt in
  print_debug_endline @@ "Broadcasting message: " ^ msg;
  broadcast @@ "M" ^ (msg |> String.length |> string_of_int) ^ "\n" ^ msg ^ "\n"

let send_gossip oc =
  let gossip = Hashtbl.fold (fun k _ s -> s ^ ";" ^ k) connections "" in
  let gossip = "G" ^ gossip ^ "\n" in
  print_debug_endline @@ "Sending gossip: " ^ gossip;
  output_string oc gossip; flush oc

let broadcast_gossip () =
  let gossip = Hashtbl.fold (fun k _ s -> s ^ ";" ^ k) connections "" in
  print_debug_endline @@ "Broadcasting gossip: " ^ gossip;
  broadcast @@ "G" ^ gossip ^ "\n"

let broadcast_drop id =
  print_debug_endline @@ "Broadcasting drop: " ^ id;
  broadcast @@ "D" ^ id ^ "\n"

let rec open_connection cb id =
  match Str.split (Str.regexp ":") id with
  | [addr; port] ->
      begin
        print_debug_endline @@ "Connecting to " ^ addr ^ ":" ^ port;
        let open Unix in
        let sock = socket PF_INET SOCK_STREAM 0 in
        setsockopt sock SO_REUSEADDR true;
        let remote_addr = ADDR_INET (inet_addr_of_string addr,
                                     int_of_string port) in
        connect sock remote_addr;
        accept_connection cb false (sock, remote_addr)
      end
  | _ -> ()

and update_connection cb c =
  if Hashtbl.mem connections c then ()
  else open_connection cb c |> ignore

and handle_gossip cb gossip =
  print_debug_endline @@ "Received gossip: " ^ gossip;
  let conns = Str.split (Str.regexp ";") gossip in
    List.iter (update_connection cb) conns

and handle_msg cb id ic msg_len =
  match safe_int_of_string msg_len with
  | Some len ->
      begin
        if len > 0 then
          let ciphertext = really_input_string ic len in
          print_debug_endline @@ "Received: " ^ ciphertext;
          (* TODO: handle gracefully *)
          let package = ciphertext |> decrypt |> deserialize in
            (fst cb) id package
        else print_debug_endline @@ "Message length cannot be 0"
      end
  | None -> print_debug_endline @@ "Message length not an integer: " ^ msg_len

and handle_drop cb id =
  try
    let (ic, oc, fd) = Hashtbl.find connections id in
    shutdown_connection ic oc fd;
    Hashtbl.remove connections id;
    (snd cb) id
  with
  | Not_found -> ()

and handle_line cb id ic line =
  let line = String.trim line in
  if String.length line > 0 then
    let data = String.sub line 1 (String.length line - 1) in
    match String.get line 0 with
    | 'G' -> handle_gossip cb data
    | 'M' -> handle_msg cb id ic data
    | 'D' -> handle_drop cb data
    | c -> print_debug_endline @@ "Received unknown header: " ^ (String.make 1 c)
  else print_debug_endline @@ "Received invalid line: " ^ line

and handle_connection cb id ic () =
  try
    let line = input_line ic in
    handle_line cb id ic line |> handle_connection cb id ic
  with
  | End_of_file ->
      begin
        print_debug_endline @@ "Connection dropped: " ^ id;
        handle_drop cb id
      end

and handle_identity oc line =
  let line = String.trim line in
  if String.length line > 0 then
    let id = String.sub line 1 (String.length line - 1) in
    match String.get line 0 with
    | 'I' -> id
    | c -> ""
  else ""

and shutdown_connection ic oc fd =
  try
    Unix.shutdown fd Unix.SHUTDOWN_ALL;
    close_in_noerr ic;
    close_out_noerr oc
  with
  | Unix.Unix_error _ -> ()

and accept_connection cb new_client (fd, _) =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  output_string oc ("I" ^ !my_id ^ "\n"); flush oc;
  let id = input_line ic |> handle_identity oc in
  if id = "" || Hashtbl.mem connections id
  then shutdown_connection ic oc fd
  else begin
    print_debug_endline @@ "New connection: " ^ id;
    Hashtbl.add connections id (ic, oc, fd);
    if new_client then broadcast_gossip ();
    send_gossip oc;
    Lwt_preemptive.detach (fun () ->
      try
        handle_connection cb id ic ()
      with
      | Unix.Unix_error (e, _, _) -> print_endline @@ Unix.error_message e;
    ) () |> ignore
  end

(* Adapted from: http://baturin.org/code/lwt-counter-server/ *)
let network_initialize port cb host_addr =
  my_id := (my_address ^ ":" ^ (string_of_int port));
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR;Unix.O_NONBLOCK] 0 in
  let ic = Unix.in_channel_of_descr dev_null in
  let oc = Unix.out_channel_of_descr dev_null in
  Hashtbl.add connections !my_id (ic, oc, dev_null);
  if host_addr <> "" then open_connection cb host_addr else ();
  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  Unix.(setsockopt sock SO_REUSEADDR true);
  Unix.(bind sock @@ ADDR_INET (my_inet_address, port));
  Unix.listen sock 4;
  let rec serve () = Unix.accept sock |> (accept_connection cb true) |> serve in
  serve ()
