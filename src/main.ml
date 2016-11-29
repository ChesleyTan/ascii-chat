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

let main () =
    Arg.parse specs ignore help_header;
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
        restore_cursor ();
        outline layout;
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
