let a = Array.make 2 1.23 in
a.(0) <- 4.56;
print_int (truncate( a.(0) ))
