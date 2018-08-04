open Notty
open Notty_lwt
open Lwt

(* ocamlfind ocamlc -o simple_lwt_terminal_resize -package notty.lwt -linkpkg -g common.ml basics_Lwt_Term_simple_terminal_resize.ml*)

module T = Notty_lwt.Term

let grid xxs = xxs |> List.map I.hcat |> I.vcat
let counter = ref 0

let outline attr t =
  let (w, h) = T.size t in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar  = I.uchar attr (Uchar.of_int 0x2500) (w - 2) 1
  and vbar  = I.uchar attr (Uchar.of_int 0x2502) 1 (h - 2) in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [ [a; hbar; b]; [vbar; I.void (w - 2) 1; vbar]; [d; hbar; c] ]

let outline_dim attr (w, h) =
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

let timer = function
  | None   -> Lwt.wait () |> fst
  | Some t -> Lwt_unix.sleep t >|= fun _ -> `Timer

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> x
  | None -> `End

let term_lwt_timed ?delay ~f =
  let term = T.create () in
  let rec loop (e, t) dim =
    (e <?> t) >>= function
    | `End | `Key (`Escape, []) | `Key (`ASCII 'C', [`Ctrl]) ->
        Lwt.return_unit
    | `Resize dim as evt     -> invoke (event term, t) dim evt
    | #Unescape.event as evt -> invoke (event term, t) dim evt
    | `Timer as evt          -> invoke (e, timer delay) dim evt
  and invoke es dim e =
    match f dim e with
    | `Continue      -> loop es dim
    | `Redraw img -> T.image term img >>= fun () -> loop es dim
    | `Stop          -> Lwt.return_unit in
  let size = T.size term in
  loop (event term, timer delay) size

let f (w,h as dim) = function
    | `Resize _| `Timer ->
        let message = size_box w h in
        `Redraw (I.((outline_dim A.(fg lightred ) dim) </> message ))
    | _ -> `Continue

let () =
  Lwt_main.run @@ term_lwt_timed ~delay:1.0 ~f
