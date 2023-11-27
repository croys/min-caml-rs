let rec compose f g =
  let rec composed x = g (f x) in
  composed in
let rec inc x = x + 1 in
let h = compose inc inc in
print_int (h 123)
