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
                        else if r = 41 then
                            if c > 106 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
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
                            else if r = 42 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else if r = 33 then
                            grid.(r).(c) <- "-"
                        else if r = 42 then
                            grid.(r).(c) <- "_"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Three ->
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
                        else if c = 54 || c = 55 || c = 109 || c = 110 then
                            if r < 22 then
                                grid.(r).(c) <- "|"
                            else if r = 22 then
                                grid.(r).(c) <- "+"
                            else if r = 42 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else if r = 22 then
                            grid.(r).(c) <- "-"
                        else if r = 42 then
                            grid.(r).(c) <- "_"
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done
        | Four ->
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
                        else if c = 54 || c = 55 || c = 109 || c = 110 then
                            if r <> 22 then
                                grid.(r).(c) <- "|"
                            else
                                grid.(r).(c) <- "+"
                        else if r = 22 then
                            if c < 109 then
                                grid.(r).(c) <- "-"
                            else
                                grid.(r).(c) <- " "
                        else if r = 41 then
                            if c > 110 then
                                grid.(r).(c) <- "_"
                            else
                                grid.(r).(c) <- " "
                        else
                            grid.(r).(c) <- " "
                    end
                done;
            done

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

let print_to_grid (start_row, start_col) (max_width, max_height) s =
    (* NOTE: We are assuming that the provided string has no format characters
     * that can interfere with the formatting, such as newlines and carriage
     * returns *)
    if start_row + max_height >= max_rows
    || start_col + max_width >= max_cols then
        failwith "print_to_grid overflow!"
    else
        let r = ref start_row
        and c = ref start_col in
        for i = 0 to (String.length s - 1) do
            grid.(!r).(!c) <- String.sub s i 1;
            incr c;
            if !c - start_col >= max_width then
                begin
                    c := start_col;
                    incr r
                end
            else ();
            if !r - start_row >= max_height then
                r := start_row
            else ();
        done

let print_grid () =
    Array.map (fun xs -> Array.to_list xs |> String.concat "") grid
    |> Array.to_list |> String.concat "\n"
    |> print_unbuf

let image_dimensions window = match window with
    | One -> (105, 43)
    | Two -> (80, 32)
    | Three -> (53, 21)
    | Four -> (53, 21)

let text_dimensions window = match window with
    | One -> (57, 40)
    | Two -> (163, 8)
    | Three -> (163, 19)
    | Four -> (52, 40)

let input_dimensions window = match window with
    | One -> (57, 2)
    | Two -> (163, 1)
    | Three -> (163, 1)
    | Four -> (52, 2)

let pane_start_coord pane window = match (pane, window) with
    | 1, One -> (1, 1)
    | 2, One -> (1, 107)
    | 3, One -> (42, 107)
    | 1, Two -> (1, 1)
    | 2, Two -> (1, 83)
    | 3, Two -> (34, 1)
    | 4, Two -> (43, 1)
    | 1, Three -> (1, 1)
    | 2, Three -> (1, 56)
    | 3, Three -> (1, 111)
    | 4, Three -> (23, 1)
    | 5, Three -> (43, 1)
    | 1, Four -> (1, 1)
    | 2, Four -> (1, 56)
    | 3, Four -> (23, 1)
    | 4, Four -> (23, 56)
    | 5, Four -> (1, 111)
    | 6, Four -> (42, 111)
    | _ -> failwith "Invalid pane number for window layout!"
