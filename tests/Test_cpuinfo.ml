open OUnit2
open Lwt.Infix
open Minotaure_lib
open System_info

let cpu_reference = "cpu 70218 850 19676 1415000 43324 9357 2362 0 0 0"
let core0_reference = "cpu0 70218 850 19676 1415000 43324 9357 2362 0 0 0"

let printer = string_of_int

let test_cpu_line_to_cpuinfo test_ctxt =
  let msg = "This should not have been reached" in
  match Cpu.line_to_cpuinfo cpu_reference with
  | None -> assert_equal ~msg false true
  | Some cpuinfo' ->
    let () = assert_equal ~printer 70218 cpuinfo'.user in
    let () = assert_equal ~printer 850 cpuinfo'.nice in
    let () = assert_equal ~printer 19676 cpuinfo'.system in
    let () = assert_equal ~printer 1415000 cpuinfo'.idle in
    let () = assert_equal ~printer 43324 cpuinfo'.iowait in
    let () = assert_equal ~printer 9357 cpuinfo'.irq in
    let () = assert_equal ~printer 2362 cpuinfo'.softirq in
    let () = assert_equal ~printer 0 cpuinfo'.steal in
    let () = assert_equal ~printer 0 cpuinfo'.guest in
    assert_equal ~printer 0 cpuinfo'.guest_nice

let test_cpu_line_to_coreinfo test_ctxt =
  let msg = "This should not have been reached test_cpuinfo_line_to_coreinfo" in
  match Cpu.line_to_coreinfo core0_reference with
  | None -> assert_equal ~msg false true
  | Some coreinfo' ->
    let () = assert_equal ~printer 0 coreinfo'.number in
    let core = coreinfo'.info in
    let () = assert_equal ~printer 70218 core.user in
    let () = assert_equal ~printer 850 core.nice in
    let () = assert_equal ~printer 19676 core.system in
    let () = assert_equal ~printer 1415000 core.idle in
    let () = assert_equal ~printer 43324 core.iowait in
    let () = assert_equal ~printer 9357 core.irq in
    let () = assert_equal ~printer 2362 core.softirq in
    let () = assert_equal ~printer 0 core.steal in
    let () = assert_equal ~printer 0 core.guest in
    assert_equal ~printer 0 core.guest_nice

let test_cpu_parse_stat_file test_ctxt =
  ignore(
    Lwt_main.run begin
  let stat_file = "../../../tests/data/proc_stat" in
  Cpu.parse_stat_file stat_file
  >>= fun cpuinfo ->
    match cpuinfo.main with
    | None -> let msg = "No info for main cpu" in
      assert_equal ~msg false true; Lwt.return_unit
    | Some info ->
      let () = assert_equal ~printer 157435 info.user in
      let () = assert_equal ~printer 330 info.nice in
      let () = assert_equal ~printer 49897 info.system in
      let () = assert_equal ~printer 2758213 info.idle in
      let () = assert_equal ~printer 98265 info.iowait in
      let () = assert_equal ~printer 9829 info.irq in
      let () = assert_equal ~printer 4885 info.softirq in
      let () = assert_equal ~printer 0 info.steal in
      let () = assert_equal ~printer 0 info.guest in
      let () = assert_equal ~printer 0 info.guest_nice in
      let () = assert_equal ~printer:string_of_int 2 (List.length cpuinfo.cores) in
      let core0 = List.nth cpuinfo.cores 1 in
      let () = assert_equal ~printer 0 core0.number in
      let core0_info = core0.info in
      let () = assert_equal ~printer 79701 core0_info.user in
      let () = assert_equal ~printer 186 core0_info.nice in
      let () = assert_equal ~printer 20803 core0_info.system in
      let () = assert_equal ~printer 1432318 core0_info.idle in
      let () = assert_equal ~printer 56172 core0_info.iowait in
      let () = assert_equal ~printer 7864 core0_info.irq in
      let () = assert_equal ~printer 3487 core0_info.softirq in
      let () = assert_equal ~printer 0 core0_info.steal in
      let () = assert_equal ~printer 0 core0_info.guest in
      let () = assert_equal ~printer 0 core0_info.guest_nice in
      Lwt.return_unit
    end
  )

let run =
  "Minotaure_lib Cpu_info tests" >:::
  [
    "test Cpu line_to_cpuinfo" >:: test_cpu_line_to_cpuinfo;
    "test Cpu line_to_coreinfo" >:: test_cpu_line_to_coreinfo;
    "test Cpu parse stat file" >:: test_cpu_parse_stat_file;
  ]
