open Notty
open Notty_unix

open Async

module Plugin = Ocaml_plugin.Dynloader.Make(struct
    type t = (module Minotaure_plugin.Plugin_intf.S)
    let t_repr = "Minotaure_plugin.Plugin_intf.S"
    let univ_constr = Minotaure_plugin.Plugin_intf.univ_constr
    let univ_constr_repr = "Minotaure_plugin.Plugin_intf.univ_constr"
  end)

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let outline attr t =
  let (w, h) = Term.size t in
  let chr x = I.uchar attr (Uchar.of_int x) 1 1
  and hbar  = I.uchar attr (Uchar.of_int 0x2500) (w - 2) 1
  and vbar  = I.uchar attr (Uchar.of_int 0x2502) 1 (h - 2) in
  let (a, b, c, d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
  grid [ [a; hbar; b]; [vbar; I.void (w - 2) 1; vbar]; [d; hbar; c] ]

let message_box message (w, h) =
  let box = I.string A.(fg lightgreen ++ bg lightblack) message in
  let top_margin = (h - I.height box) / 2 in
  let left_margin = (w - I.width box) / 2 in
  I.pad ~t:top_margin ~l:left_margin box

let rec main t message (x, y as pos) =
  let img = I.((outline A.(fg lightred ) t) </> (message_box message pos)) in
  Term.image t img;
  Term.cursor t (Some pos);
  match Term.event t with
  | `End | `Key (`Escape, []) | `Key (`ASCII 'q', []) -> return (shutdown 0)
  | `Resize (cols, rows) -> main t message (cols, rows)
  | _ -> main t message pos

let () =
  don't_wait_for (
    Ocaml_plugin.Private.Shell.set_defaults ~verbose:true ~echo:true ();
    Ocaml_plugin.Compiler.create () >>= function
    | Error e ->
      Core.Printf.eprintf "Cannot build embed loader: %s" (Core.Error.to_string_hum e);
      Core.Printf.eprintf "use run_standalone.exe (cf build.sh) instead\n%!";
      exit 1
    | Ok (`this_needs_manual_cleaning_after compiler) ->
      let loader = Ocaml_plugin.Compiler.loader compiler in
      let files = ["/home/cedlemo/Project/OCaml/minotaure/data/interface.ml"] in
      Plugin.load_ocaml_src_files loader files >>= function
      | Error err ->
        Core.Printf.eprintf "loading failed:\n%s\n%!" (Core.Error.to_string_hum err);
        exit 1
      | Ok plugin ->
        let module M = (val plugin : Minotaure_plugin.Plugin_intf.S) in
        let t = Term.create () in
        main t M.message (Term.size t)
)

let () = Core.never_returns (Scheduler.go ())
