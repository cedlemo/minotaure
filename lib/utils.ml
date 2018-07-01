open Lwt.Infix
open Lwt_unix

(* Takes a buffer and return the first occurence of a line if there is one. *)
let capture_line buff =
  let pattern = "^\\(.*\\)\n" in
  let reg = Str.regexp pattern in
  let buff_str = Bytes.to_string buff in
  match Str.string_match reg buff_str 0 with
  | true -> Some (Str.matched_group 1 buff_str)
  | false -> None

(* Returns all the lines in a buffer following the pattern of capture_line
 * /!\ it may not return the last part of the buffer if the last caracter of
 * the buffer is not \n. This is done intentionally.
 *
 * /!\ the lines are in the inverted order. *)
let rec read_lines_in_buff buffer lines =
    let rec _read_buff lines =
      match capture_line !buffer with
      | None -> lines
      | Some str ->
        let start = (String.length str + 1) in
        let len = (Bytes.length !buffer) - start in
        let () = buffer := Bytes.sub !buffer start len in
        let lines' = str :: lines in
        _read_buff lines'
  in _read_buff lines

let read_lines filename =
  let flags = [O_RDONLY] in
  let perms = 0 in
  openfile filename flags perms
  >>= fun fdesc ->
    let maxlen = 1024 in
    let buffer = ref Bytes.empty in
    let rec _read_file lines =
      let _buf = Bytes.create maxlen in
      read fdesc _buf 0 maxlen
      >>= function
      | 0 -> Lwt.return (List.rev (read_lines_in_buff buffer lines))
      | len ->
        let () = buffer := Bytes.cat !buffer _buf in
        read_lines_in_buff buffer lines |> _read_file
    in _read_file []
