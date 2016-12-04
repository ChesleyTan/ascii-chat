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

let specs = [ ("--text-only", Arg.Set text_only, "Enable text-only mode")
            ; ("-t", Arg.Set text_only, "Enable text-only mode")
            ]

let help_header = "Available options: "

let img_width = ref 0
let img_height = ref 0

(* TODO add API functions for networking module to call *)

let main () =
    Arg.parse specs ignore help_header;
    let term_attr = Unix.tcgetattr Unix.stdin in
    let term_attr_new = { term_attr with Unix.c_icanon = false
                        ; Unix.c_echo = false
                        ; Unix.c_vtime = 1
                        } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term_attr_new;
    (* TODO remove dependency on lambda-term *)
    let terminal = Lazy.force LTerm.stdout
    in terminal >>= fun term ->
    let term_size = LTerm.size term in
    let term_width = LTerm_geom.cols term_size
    and term_height = LTerm_geom.rows term_size in
    if term_width < max_cols || term_height < max_rows then
        begin
            print_endline @@ "Terminal dimensions must be at least " ^
                             (string_of_int max_cols) ^
                             " x " ^
                             (string_of_int max_rows);
            exit 1
        end
    else ();
    init_state "PLACEHOLDER_IP:PLACEHOLDER_PORT";
    let layout = get_num_users () |> layout_for_num_users in
    let img_dims = image_dimensions layout in
    img_width := fst img_dims;
    img_height := snd img_dims;
    clear_screen ();
    let _ = Lwt_preemptive.detach (fun () ->
        while true; do
            Cv.get_frame !text_only !img_width !img_height |> log_image;
            Unix.sleepf 0.1 |> ignore
        done
    ) () in
    while true; do
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
        else if char_code <> 0 then
            update_input_buffer inchar
        else ();
        restore_cursor ();
        render !text_only;
        (*
        time (fun _ -> get_frame false |> (fun x -> pack x "text" (get_timestamp
        ())) |> serialize |> compress |>
        decompress |> deserialize |> unpack |> (fun (a,b,c) -> a) |> colorize |> print_unbuf);
        *)
        Unix.sleepf 0.1;
    done;
    Cv.cleanup ();
    terminal

let _ =
    Lwt_main.run (main ())
