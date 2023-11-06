(* Note: addition to original mincaml tests *)
let a = Array.make 2 (Array.make 0 0.0) in
a.(0) <- Array.make 2 0.0;
a.(1) <- Array.make 2 0.0;
a.(0).(0) <- 0.0;
a.(0).(1) <- 1.0;
a.(1).(0) <- 2.0;
a.(1).(1) <- 3.0;
print_int (truncate a.(0).(0));
print_int (truncate a.(0).(1));
print_int (truncate a.(1).(0));
print_int (truncate a.(1).(1))
