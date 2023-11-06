let a = Array.make 2 1.23 in
a.(0) <- 4.56;
a.(1) <- 7.78;
print_int (truncate( a.(0) ));
print_int (truncate( a.(1) ))