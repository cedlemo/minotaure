open OUnit2
open Minotaure_lib
open Lwt
open Lwt.Infix


let printer = fun x -> x

let test_capture_line test_ctxt =
  let sample1 = "this is not a line" in
  let sample2 = "this is a line\n" in
  let check2 = "this is a line" in
  let sample3 = "this is the first part\n this is the second par" in
  let check3 = "this is the first part" in
  let _ = match Utils.capture_line (Bytes.of_string sample1) with
    | None -> true
    | Some _ -> false
  in
  let () = assert_equal true true in
  let t2 = match Utils.capture_line (Bytes.of_string sample2) with
    | None -> "Error"
    | Some s -> s
  in
  let () = assert_equal ~printer check2 t2 in
  let t3 = match Utils.capture_line (Bytes.of_string sample3) with
    | None -> "Error"
    | Some s -> s
  in
  assert_equal ~printer check3 t3

let test_read_lines_in_buff test_ctxt =
  let buf1 = ref (Bytes.of_string "line one\nline two\n") in
  let buf2 = ref (Bytes.of_string "line three\nline four") in
  let lines = Utils.read_lines_in_buff buf1 [] in
  let () = assert_equal ~printer:string_of_int 2 (List.length lines) in
  let () = assert_equal ~printer "line one" (List.nth lines 1) in
  let () = assert_equal ~printer "line two" (List.nth lines 0) in
  let lines = Utils.read_lines_in_buff buf2 [] in
  let () = assert_equal ~printer:string_of_int 1 (List.length lines) in
  assert_equal ~printer "line three" (List.nth lines 0)

let test_read_lines test_ctxt =
  ignore(
    Lwt_main.run (
      Utils.read_lines "../../../tests/data/file_read_lines_test"
      >>= fun lines ->
      let () = assert_equal ~printer:string_of_int 3 (List.length lines) in
      let () = assert_equal ~printer "line one" (List.nth lines 0) in
      let () = assert_equal ~printer "line two" (List.nth lines 1) in
      let () = assert_equal ~printer "line three" (List.nth lines 2) in
      Lwt.return_unit
    )
)

let run =
  "Minotaure utils tests" >:::
  [
    "Test capture line" >:: test_capture_line;
    "Test read lines in buffer" >:: test_read_lines_in_buff;
    "Test read lines" >:: test_read_lines;
  ]
