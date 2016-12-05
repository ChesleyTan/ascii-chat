open Package
open Lwt

let my_address = Utils.get_address_self()

let my_inet_address = Unix.inet_addr_of_string my_address

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) -> (Unix.string_of_inet_addr addr)
                                   ^ ":" ^ (string_of_int port)

let send package =
  failwith "Unimplemented"

let handle_data cb data =
  failwith "unimplemented"

let rec handle_connection cb ic oc () =
  Lwt_io.read_line_opt ic >>=
  (fun msg ->
     match msg with
     | Some data -> return (handle_data cb data) >>= handle_connection cb ic oc
     | None -> return (print_endline "Connection closed")
  )

let accept_connection cb (fd, remote_addr) =
  print_endline ("New connection from " ^ (string_of_sockaddr remote_addr));
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection cb ic oc ())
    (fun e -> print_endline (Printexc.to_string e));
  return ()

let network_initialize port cb =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock @@ ADDR_INET (my_inet_address, port);
  listen sock 10;
  let rec server_loop () =
    print_endline "Hello";
    accept sock >>= accept_connection cb >>= server_loop in
  server_loop() |> ignore
