(* https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux *)
type cpuinfo = {
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
    "^\\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\) \\(.*\\)"  in
  let reg = Str.regexp pattern in
  match Str.string_match reg line 0 with
  | false -> None
  | true ->
    Some {
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
