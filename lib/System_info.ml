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

 type t = {
    main  : cpuinfo;
    cores : cpuinfo list;
  }

  let line_to_cpuinfo line =
    let pattern =
      "^cpu\\([0-9]+\\)* +\\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
    let reg = Str.regexp pattern in
    match Str.string_match reg line 0 with
    | false -> None
    | true ->
      Some  {
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

  let parse_cpuinfo_file filename =
    Utils.read_lines filename
    >>= fun lines ->
    let rec fetch_data (cpu, cores) = function
      | [] -> (cpu, cores)
      | line :: rest ->
        match cpu with
        | None -> begin match line_to_cpuinfo line with
          | Some cpuinfo -> fetch_data (Some cpuinfo, cores) rest
          | None -> (cpu, cores)
          end
        | Some _ ->
            match line_to_cpuinfo line with
          | Some cpuinfo ->
            fetch_data (cpu, cpuinfo :: cores) rest
          | None -> (cpu, cores)
    in
    let (main_opt, cores) = fetch_data (None, []) lines in
    match main_opt with
    | None -> Lwt.return_error "No Cpu information found"
    | Some main -> Lwt.return_ok {main; cores}

let parse_stat_file () =
  parse_cpuinfo_file "/proc/stat"

  (* let usage cpu =
      PrevIdle = previdle + previowait
      Idle = idle + iowait

      PrevNonIdle = prevuser + prevnice + prevsystem + previrq + prevsoftirq + prevsteal
      NonIdle = user + nice + system + irq + softirq + steal

      PrevTotal = PrevIdle + PrevNonIdle
      Total = Idle + NonIdle

      # differentiate: actual value minus the previous one
      totald = Total - PrevTotal
      idled = Idle - PrevIdle

      CPU_Percentage = (totald - idled)/totald
  *)
  let usage cpuinfo =
    let non_idle = cpuinfo.user +
                   cpuinfo.nice +
                   cpuinfo.system +
                   cpuinfo.irq +
                   cpuinfo.softirq +
                   cpuinfo.steal in
    let idle = cpuinfo.idle + cpuinfo.iowait in
    let total = idle + non_idle in
    (float_of_int non_idle) /. float_of_int total

  type stats = {
    overall : float;
    cores: float list;
  }

  let stats cpu =
    let u = cpu.main in
    let overall = usage u in
    let rec cores_usage acc = function
      | [] -> List.rev acc
      | c :: cs -> cores_usage ((usage c) :: acc) cs
    in

    { overall = overall;
      cores = cores_usage [] cpu.cores;
    }
end
