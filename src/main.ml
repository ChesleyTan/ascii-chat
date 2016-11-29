open Arg
open Lwt
open Cv
open Package
open View

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

let my_image = ref { data = (FastString.of_string "")
                   ; colors = [|""|]
                   ; width = 0
                   ; height = 0
                   ; text_only  = true
                   }

let input_buffer = ref ""

let main () =
    Arg.parse specs ignore help_header;
    let term_attr = Unix.tcgetattr Unix.stdin in
    let term_attr_new = { term_attr with Unix.c_icanon = false
                        ; Unix.c_echo = false
                        ; Unix.c_vtime = 1
                        } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term_attr_new;
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
    let layout = Four in
    let img_dims = image_dimensions layout in
    let img_width = ref @@ fst img_dims
    and img_height = ref @@ snd img_dims in
    clear_screen ();
    let _ = Lwt_preemptive.detach (fun () ->
        while true; do
            my_image := Cv.get_frame !text_only !img_width !img_height;
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
        and buffer_length = String.length !input_buffer in
        let char_code = Char.code inchar in
        (* Handle backspace *)
        if char_code = 8 || char_code = 127 then
            if buffer_length > 0 then
                input_buffer := String.sub !input_buffer 0 (buffer_length - 1)
            else ()
        (* TODO handle enter *)
        else if char_code <> 0 && char_code <> 10 then
            input_buffer := !input_buffer ^ (String.make 1 inchar)
        else ();
        restore_cursor ();
        outline layout;
        print_to_grid (pane_start_coord 5 layout) (text_dimensions layout)
            !input_buffer;
        !my_image |> Cv.colorize !text_only
                  |> copy_to_grid (pane_start_coord 1 layout);
        !my_image |> Cv.colorize !text_only
                  |> copy_to_grid (pane_start_coord 2 layout);
        !my_image |> Cv.colorize !text_only
                  |> copy_to_grid (pane_start_coord 3 layout);
        !my_image |> Cv.colorize !text_only
                  |> copy_to_grid (pane_start_coord 4 layout);
        print_grid ();
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
