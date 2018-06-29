open Notty
open Notty_lwt
open Lwt

(* ocamlfind ocamlc -o simple_lwt_terminal_resize -package notty.lwt -linkpkg -g common.ml basics_Lwt_Term_simple_terminal_resize.ml*)


module LwtTerm = Notty_lwt.Term

let grid xxs = xxs |> List.map I.hcat |> I.vcat
let counter = ref 0

let outline attr t =
  let (w, h) = LwtTerm.size t in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar  = I.uchar attr (Uchar.of_int 0x2500) (w - 2) 1
  and vbar  = I.uchar attr (Uchar.of_int 0x2502) 1 (h - 2) in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [ [a; hbar; b]; [vbar; I.void (w - 2) 1; vbar]; [d; hbar; c] ]

let size_box cols rows =
  let cols_str = string_of_int cols in let rows_str = string_of_int rows in
  let label = String.concat " " [cols_str; "x" ; rows_str; string_of_int !counter] in
  let box = I.string A.(fg lightgreen ++ bg lightblack) label in
  let top_margin = (rows - I.height box) / 2 in
  let left_margin = (cols - I.width box) / 2 in
  I.pad ~t:top_margin ~l:left_margin box

let timer () = Lwt_unix.sleep 1.0 >|= fun () -> `Timer
let event term = Lwt_stream.get (Term.events term) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let rec main term (e, t) (x, y as pos) =
  let img = I.((outline A.(fg lightred ) term) </> (size_box x y)) in
  LwtTerm.image term img
  >>= fun () ->
    LwtTerm.cursor term (Some pos)
    >>= fun () ->
      (e<?>t) >>= function
        | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) -> LwtTerm.release term >>= fun () -> Lwt.return_unit
        | `Resize (cols, rows) -> main term (e,t) (cols, rows)
        | `Timer -> let () = counter := !counter + 1 in
            main term (e,timer()) (x, y)
        | _ ->Lwt.return () >>= fun () -> main term (e,t) pos

let () =
  let term = LwtTerm.create () in
  let size = LwtTerm.size term in
  Lwt_main.run @@ main term (event term, timer ()) size
