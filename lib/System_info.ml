open Lwt.Infix

(* https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux *)

type cpuinfo =  {
  user : string;
  nice : string;
  system : string;
  idle : string;
  iowait : string;
  irq : string;
  softirq : string;
  steal : string;
  guest : string;
  guest_nice : string;
}

type coreinfo = {
  number : string;
  user : string;
  nice : string;
  system : string;
  idle : string;
  iowait : string;
  irq : string;
  softirq : string;
  steal : string;
  guest : string;
  guest_nice : string;
}

let line_to_cpuinfo line =
  let pattern =
    "^cpu +\\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
  let reg = Str.regexp pattern in
  match Str.string_match reg line 0 with
  | false -> None
  | true ->
    Some {
      user = Str.matched_group 1 line;
      nice = Str.matched_group 2 line;
      system = Str.matched_group 3 line;
      idle = Str.matched_group 4 line;
      iowait = Str.matched_group 5 line;
      irq = Str.matched_group 6 line;
      softirq = Str.matched_group 7 line;
      steal = Str.matched_group 8 line;
      guest = Str.matched_group 9 line;
      guest_nice = Str.matched_group 10 line;
    }

let line_to_coreinfo line =
  let pattern =
    "^cpu\\([0-9]+\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
  let reg = Str.regexp pattern in
  match Str.string_match reg line 0 with
  | false -> None
  | true ->
    Some {
      number = Str.matched_group 1 line;
      user = Str.matched_group 2 line;
      nice = Str.matched_group 3 line;
      system = Str.matched_group 4 line;
      idle = Str.matched_group 5 line;
      iowait = Str.matched_group 6 line;
      irq = Str.matched_group 7 line;
      softirq = Str.matched_group 8 line;
      steal = Str.matched_group 9 line;
      guest = Str.matched_group 10 line;
      guest_nice = Str.matched_group 11 line;
    }

type cpu = {
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
