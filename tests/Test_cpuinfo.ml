open OUnit2
open Minotaure_lib

let cpu_reference = "cpu 70218 850 19676 1415000 43324 9357 2362 0 0 0"
let core0_reference = "cpu0 70218 850 19676 1415000 43324 9357 2362 0 0 0"

let printer = fun x -> x

let test_cpuinfo_line_to_cpuinfo test_ctxt =
  let msg = "This should not have been reached" in
  match Cpu_info.line_to_cpuinfo cpu_reference with
  | None -> assert_equal ~msg false true
  | Some cpuinfo' ->
    let () = assert_equal ~printer "70218" cpuinfo'.user in
    let () = assert_equal ~printer "850" cpuinfo'.nice in
    let () = assert_equal ~printer "19676" cpuinfo'.system in
    let () = assert_equal ~printer "1415000" cpuinfo'.idle in
    let () = assert_equal ~printer "43324" cpuinfo'.iowait in
    let () = assert_equal ~printer "9357" cpuinfo'.irq in
    let () = assert_equal ~printer "2362" cpuinfo'.softirq in
    let () = assert_equal ~printer "0" cpuinfo'.steal in
    let () = assert_equal ~printer "0" cpuinfo'.guest in
    assert_equal ~printer "0" cpuinfo'.guest_nice

let test_cpuinfo_line_to_coreinfo test_ctxt =
  let msg = "This should not have been reached test_cpuinfo_line_to_coreinfo" in
  match Cpu_info.line_to_coreinfo core0_reference with
  | None -> assert_equal ~msg false true
  | Some coreinfo' ->
    let () = assert_equal ~printer "0" coreinfo'.number in
    let () = assert_equal ~printer "70218" coreinfo'.user in
    let () = assert_equal ~printer "850" coreinfo'.nice in
    let () = assert_equal ~printer "19676" coreinfo'.system in
    let () = assert_equal ~printer "1415000" coreinfo'.idle in
    let () = assert_equal ~printer "43324" coreinfo'.iowait in
    let () = assert_equal ~printer "9357" coreinfo'.irq in
    let () = assert_equal ~printer "2362" coreinfo'.softirq in
    let () = assert_equal ~printer "0" coreinfo'.steal in
    let () = assert_equal ~printer "0" coreinfo'.guest in
    assert_equal ~printer "0" coreinfo'.guest_nice

let run =
  "Minotaure_lib Cpu_info tests" >:::
  ["test cpuinfo line_to_cpuinfo" >:: test_cpuinfo_line_to_cpuinfo;
   "test coreinfo line_to_coreinfo" >:: test_cpuinfo_line_to_coreinfo;
  ]
