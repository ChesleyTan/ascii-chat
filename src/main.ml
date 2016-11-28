open Cv.Cv
open Package
open Arg
open Lwt

let print_unbuf s =
    Printf.printf "%s%!" s

let clear_screen () =
    print_unbuf "\x1B[2J"

let restore_cursor () =
    print_unbuf "\x1B[;H"

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

let main () =
    Arg.parse specs ignore help_header;
    let terminal =
        LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout
    in terminal >>= fun term ->
    let term_size = LTerm.size term in
    let term_width = LTerm_geom.cols term_size
    and term_height = LTerm_geom.rows term_size in
    if term_width < 160 || term_height < 42 then
        begin
            print_endline "Terminal dimensions must be at least 160 x 42!";
            exit 1
        end
    else ();
    clear_screen ();
    while true; do
        restore_cursor ();
        time (fun _ -> get_frame !text_only 80 32 |> colorize |> print_unbuf);
        (*
        time (fun _ -> get_frame false |> (fun x -> pack x "text" (get_timestamp
        ())) |> serialize |> compress |>
        decompress |> deserialize |> unpack |> (fun (a,b,c) -> a) |> colorize |> print_unbuf);
        *)
        Unix.sleepf 0.1;
    done;
    cleanup ();
    terminal

let _ = Lwt_main.run (main ())
