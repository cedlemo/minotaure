open Lwt.Infix

(* https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux *)

module Cpu = struct
  type cpuinfo =  {
    user : int;
    nice : int;
    system : int;
    idle : int;
    iowait : int;
    irq : int;
    softirq : int;
    steal : int;
    guest : int;
    guest_nice : int;
  }

  type coreinfo = {
    number : int;
    user : int;
    nice : int;
    system : int;
    idle : int;
    iowait : int;
    irq : int;
    softirq : int;
    steal : int;
    guest : int;
    guest_nice : int;
  }

  let line_to_cpuinfo line =
    let pattern =
      "^cpu +\\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
    let reg = Str.regexp pattern in
    match Str.string_match reg line 0 with
    | false -> None
    | true ->
      Some {
        user = Str.matched_group 1 line |> int_of_string;
        nice = Str.matched_group 2 line |> int_of_string;
        system = Str.matched_group 3 line |> int_of_string;
        idle = Str.matched_group 4 line |> int_of_string;
        iowait = Str.matched_group 5 line |> int_of_string;
        irq = Str.matched_group 6 line |> int_of_string;
        softirq = Str.matched_group 7 line |> int_of_string;
        steal = Str.matched_group 8 line |> int_of_string;
        guest = Str.matched_group 9 line |> int_of_string;
        guest_nice = Str.matched_group 10 line |> int_of_string;
      }

  let line_to_coreinfo line =
    let pattern =
      "^cpu\\([0-9]+\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
    let reg = Str.regexp pattern in
    match Str.string_match reg line 0 with
    | false -> None
    | true ->
      Some {
        number = Str.matched_group 1 line |> int_of_string;
        user = Str.matched_group 2 line |> int_of_string;
        nice = Str.matched_group 3 line |> int_of_string;
        system = Str.matched_group 4 line |> int_of_string;
        idle = Str.matched_group 5 line |> int_of_string;
        iowait = Str.matched_group 6 line |> int_of_string;
        irq = Str.matched_group 7 line |> int_of_string;
        softirq = Str.matched_group 8 line |> int_of_string;
        steal = Str.matched_group 9 line |> int_of_string;
        guest = Str.matched_group 10 line |> int_of_string;
        guest_nice = Str.matched_group 11 line |> int_of_string;
      }

  type t = {
    main : cpuinfo option;
    cores : coreinfo list;
  }

  let parse_stat_file filename =
    Utils.read_lines filename
    >>= fun lines ->
    let rec fetch_data cpu = function
      | [] -> Lwt.return cpu
      | line :: rest ->
        match line_to_cpuinfo line with
        | Some cpuinfo ->
          fetch_data {cpu with main = Some cpuinfo} rest
        | None ->
          match line_to_coreinfo line with
          | Some coreinfo ->
            fetch_data {cpu with cores = (coreinfo :: cpu.cores)} rest
          | None ->
            fetch_data cpu rest
    in
    fetch_data {main=None; cores = []} lines


end
