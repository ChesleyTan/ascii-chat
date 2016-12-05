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

(* Adapted from: http://baturin.org/code/lwt-counter-server/ *)
let rec handle_connection fd =
    let buf = Bytes.create 1 in
    let rec inner () =
    Unix.read fd buf 0 1 |>
    (fun n -> if n <> 0 then print_endline buf |> inner else return ())
    in inner

let accept_connection conn =
    let open Unix in
    let fd, _ = conn in
    print_endline "new connection";
    Lwt_preemptive.detach (fun () ->
        handle_connection fd ()
    ) ();
    ()

let create_socket port =
    let open Unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR true;
    bind sock @@ ADDR_INET(my_inet_address, port);
    listen sock 10;
    sock

let network_initialize port cb =
  let sock = create_socket port in
  let rec serve () = Unix.accept sock |> accept_connection |> serve in
  serve()
