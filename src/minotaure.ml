open Notty
open Notty_unix

open Async

let builddir = Filename.concat (Core.Sys.getcwd()) "_build"

module Plugin = Ocaml_plugin.Dynloader.Make(struct
    type t = (module Ocaml_plugin_minotaure.Plugin_intf.S)
    let t_repr = "Ocaml_plugin_minotaure.Plugin_intf.S"
    let univ_constr = Ocaml_plugin_minotaure.Plugin_intf.univ_constr
    let univ_constr_repr = "Ocaml_plugin_minotaure.Plugin_intf.univ_constr"
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
    Sys.getcwd () >>= fun cwd ->
    let in_dir = Filename.concat cwd "tmp_dir" in
    let ocamlopt_opt = "ocamlopt.opt" in
    let include_directories =
    (* Those directories must point to the lib dir where lies the ocaml_plugin_minotaure.cmi
     * and the lib dir where is the ocaml_plugin.cmi *)
    [ "."; "../lib"; "/home/cedlemo/Projets/OCaml/minotaure/src" ;
    "/home/cedlemo/Projets/OCaml/minotaure/lib" ;
    "/home/cedlemo/.opam/4.06.0/lib/ocaml_plugin/"] in
    let files = ["/home/cedlemo/Projets/OCaml/minotaure/data/interface.ml"] in
    Ocaml_plugin.Dynloader.create ~in_dir ~include_directories ~ocamlopt_opt () >>= function
    | Error err -> Core.Printf.eprintf "loading failed:\n%s\n%!" (Core.Error.to_string_hum err);
        exit 1
    | Ok loader -> Plugin.load_ocaml_src_files loader files >>= function
      | Error err ->
        Core.Printf.eprintf "loading failed:\n%s\n%!" (Core.Error.to_string_hum err);
        exit 1
      | Ok plugin ->
        let module M = (val plugin : Ocaml_plugin_minotaure.Plugin_intf.S) in
        let t = Term.create () in
        main t M.message (Term.size t)

)

let () = Core.never_returns (Scheduler.go ())
