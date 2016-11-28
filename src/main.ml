open Cv.Cv
open Package

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

let _ =
    clear_screen ();
    while true; do
        restore_cursor ();
        time (fun _ -> get_frame false |> colorize |> print_unbuf);
        (*
        time (fun _ -> get_frame false |> (fun x -> pack x "text") |> serialize |> compress |>
        decompress |> deserialize |> unpack |> fst |> colorize |> print_unbuf);
        *)
        Unix.sleepf 0.1;
    done;
    cleanup ()
