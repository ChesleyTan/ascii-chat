open Arg
open Lwt
open Cv
open Package
open View
open State

let time f =
    let start = Unix.gettimeofday () in
    let ret = f () in
    let stop = Unix.gettimeofday () in
    print_unbuf @@ Printf.sprintf "Execution time: %fs\n" (stop -. start);
    ret

let text_only = ref false
let cryptokey = ref ""

let specs = [ ("--text-only", Arg.Set text_only, "Enable text-only mode")
            ; ("-t", Arg.Set text_only, "Enable text-only mode")
            ]

let help_header = "Available options: "

let img_width = ref 0
let img_height = ref 0

(* TODO add API functions for networking module to call *)
(* TODO add MACS and DH key exchange *)

(* Adapted from: http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
 * Section "Determining Terminal or Window Size"
 *)
let get_terminal_dimensions () =
    let input_channel = Unix.open_process_in "stty size" in
    let ib = Scanf.Scanning.from_channel input_channel in
    try
        Scanf.bscanf ib "%d %d"
            (fun rows cols ->
                Unix.close_process_in input_channel |> ignore;
                Some (rows, cols)
            )
    with _ -> None

let check_terminal_dimensions () =
    match get_terminal_dimensions () with
        | Some (rows, cols) when cols < max_cols || rows < max_rows ->
            print_endline @@ "Terminal dimensions must be at least " ^
                            (string_of_int max_cols) ^
                            " x " ^
                            (string_of_int max_rows);
            exit 1
        | _ -> ()

(* Adapted from: http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
 * Section: "Reading from the Keyboard"
 *)
let set_non_canonical_term () =
    let term_attr = Unix.tcgetattr Unix.stdin in
    let term_attr_new = { term_attr with Unix.c_icanon = false
                        ; Unix.c_echo = false
                        ; Unix.c_vtime = 1
                        } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term_attr_new

let handle_key_input () =
    let inchar =
        try
            begin
                Unix.set_nonblock Unix.stdin;
                let res = input_char stdin in
                Unix.clear_nonblock Unix.stdin; res
            end
        with
            | Sys_blocked_io ->
                begin
                    Unix.clear_nonblock Unix.stdin;
                    '\x00'
                end
    and buffer_length = get_input_buffer_length () in
    let char_code = Char.code inchar in
    (* Handle backspace *)
    if char_code = 8 || char_code = 127 then
        if buffer_length > 0 then
            delete_input_buffer ()
        else ()
    else if char_code = 10 then
        log_message ()
    else if char_code <> 0 && char_code <> 27 then
        update_input_buffer inchar
    else ()

let main () =
    Arg.parse specs generate_encryption_key help_header;
    if not @@ is_encryption_key_set () then
        begin
            print_endline @@ "You must specify a key for encryption.\n" ^
                            "Usage: ./ascii-chat <key>";
            exit 1
        end
    else ();
    set_non_canonical_term ();
    check_terminal_dimensions ();
    (* TODO get the user's ip and port *)
    init_state "PLACEHOLDER_IP:PLACEHOLDER_PORT";
    clear_screen ();
    let _ = Lwt_preemptive.detach (fun () ->
        while true; do
            let layout = get_num_users () |> layout_for_num_users in
            let img_dims = image_dimensions layout in
            img_width := fst img_dims;
            img_height := snd img_dims;
            Cv.get_frame !text_only !img_width !img_height |> log_image;
            Unix.sleepf 0.1 |> ignore
        done
    ) () in
    while true; do
        handle_key_input ();
        restore_cursor ();
        render !text_only;
        Unix.sleepf 0.1;
    done;
    Cv.cleanup ();
    return ()

let _ =
    Lwt_main.run (main ())
