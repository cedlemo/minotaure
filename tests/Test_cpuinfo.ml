open OUnit2
open Lwt.Infix
open Minotaure_lib

let cpu_reference = "cpu 70218 850 19676 1415000 43324 9357 2362 0 0 0"
let core0_reference = "cpu0 70218 850 19676 1415000 43324 9357 2362 0 0 0"

let printer = fun x -> x

let test_cpuinfo_line_to_cpuinfo test_ctxt =
  let msg = "This should not have been reached" in
  match System_info.line_to_cpuinfo cpu_reference with
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
  match System_info.line_to_coreinfo core0_reference with
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

let test_cpuinfo_parse_stat_file test_ctxt =
  ignore(
    Lwt_main.run begin
  let stat_file = "../../../tests/data/proc_stat" in
  System_info.parse_stat_file stat_file
  >>= fun cpuinfo ->
    match cpuinfo.main with
    | None -> let msg = "No info for main cpu" in
      assert_equal ~msg false true; Lwt.return_unit
    | Some info ->
      let () = assert_equal ~printer "157435" info.user in
      let () = assert_equal ~printer "330" info.nice in
      let () = assert_equal ~printer "49897" info.system in
      let () = assert_equal ~printer "2758213" info.idle in
      let () = assert_equal ~printer "98265" info.iowait in
      let () = assert_equal ~printer "9829" info.irq in
      let () = assert_equal ~printer "4885" info.softirq in
      let () = assert_equal ~printer "0" info.steal in
      let () = assert_equal ~printer "0" info.guest in
      let () = assert_equal ~printer "0" info.guest_nice in
      let () = assert_equal ~printer:string_of_int 2 (List.length cpuinfo.cores) in
      let core0 = List.nth cpuinfo.cores 1 in
      let () = assert_equal ~printer "0" core0.number in
      let () = assert_equal ~printer "79701" core0.user in
      let () = assert_equal ~printer "186" core0.nice in
      let () = assert_equal ~printer "20803" core0.system in
      let () = assert_equal ~printer "1432318" core0.idle in
      let () = assert_equal ~printer "56172" core0.iowait in
      let () = assert_equal ~printer "7864" core0.irq in
      let () = assert_equal ~printer "3487" core0.softirq in
      let () = assert_equal ~printer "0" core0.steal in
      let () = assert_equal ~printer "0" core0.guest in
      let () = assert_equal ~printer "0" core0.guest_nice in
      Lwt.return_unit
    end
  )

let run =
  "Minotaure_lib Cpu_info tests" >:::
  [
    "test cpuinfo line_to_cpuinfo" >:: test_cpuinfo_line_to_cpuinfo;
    "test coreinfo line_to_coreinfo" >:: test_cpuinfo_line_to_coreinfo;
    "test cpuinfo parse stat file" >:: test_cpuinfo_parse_stat_file;
  ]
