open OUnit2
open Minotaure_lib

let test_capture_line test_ctxt =
  let printer = fun x -> x in
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

let run =
  "Minotaure utils tests" >:::
  [
    "Test capture line" >:: test_capture_line;
  ]
