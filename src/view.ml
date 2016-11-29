open Lwt

(* The number of panes in a window *)
type window =  One | Two | Three | Four

let max_cols = 165
let max_rows = 45

let grid = Array.make_matrix max_rows max_cols ""

let print_unbuf s =
    Printf.printf "%s%!" s

let clear_screen () =
    print_unbuf "\x1B[2J"

let restore_cursor () =
    print_unbuf "\x1B[;H"

let outline window =
    match window with
        | One ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 106 then
                            grid.(r).(c) <- "|"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Two ->
            for r = 0 to (max_rows - 1) do
                for c = 0 to (max_cols - 1) do
                    begin
                        if r = 0 || r = (max_rows - 1) then
                            if c = 0 || c = (max_cols - 1) then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- "-"
                        else if c = 0 || c = (max_cols - 1) then
                            if r <> 0 && r <> (max_rows - 1) then
                                grid.(r).(c) <- "|"
                            else ()
                        else if c = 81 || c = 82 then
                            if r < 33 then
                                grid.(r).(c) <- "|"
                            else if r = 33 then
                                grid.(r).(c) <- "+"
                            else
                                grid.(r).(c) <- " "
                        else if r = 33 then
                            grid.(r).(c) <- "-"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | _ -> failwith "unimplemented"

let copy_to_grid (start_row, start_col) g =
    let g_rows = (Array.length g) - 1 in
    if g_rows < 0 then
        ()
    else
        let g_cols = (Array.length g.(0)) - 1 in
        if start_row + g_rows >= max_rows || start_col + g_cols >= max_cols then
            failwith "copy_to_grid overflow!"
        else
            for r = 0 to g_rows do
                for c = 0 to g_cols do
                    grid.(start_row + r).(start_col + c) <- g.(r).(c)
                done
            done

let print_grid () =
    Array.map (fun xs -> Array.to_list xs |> String.concat "") grid
    |> Array.to_list |> String.concat "\n"
    |> print_unbuf

let image_dimensions window = match window with
    | One -> (105, 43)
    | Two -> (80, 32)
    | _ -> failwith "unimplemented"

let pane_start_coord pane window = match (pane, window) with
    | 1, One -> (1, 1)
    | 2, One -> (1, 107)
    | 1, Two -> (1, 1)
    | 2, Two -> (1, 83)
    | 3, Two -> (34, 1)
    | _ -> failwith "unimplemented"
