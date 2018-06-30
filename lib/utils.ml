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

let read_lines filename =
  let flags = [O_RDONLY] in
  let perms = 0 in
  openfile filename flags perms
  >>= fun fdesc ->
    let maxlen = 1024 in
    let buffer = ref Bytes.empty in
    let rec _read lines =
      let _buf = Bytes.create maxlen in
      read fdesc _buf 0 maxlen
      >>= function
      | 0 -> Lwt.return (List.rev lines)
      | len ->
        let () = buffer := Bytes.cat !buffer _buf in
        match capture_line !buffer with
        | None -> _read lines
        | Some str ->
          let len = String.length str in
          let () = buffer := Bytes.sub !buffer 0 len in
          let lines' = str :: lines in
          _read lines'
    in _read []
