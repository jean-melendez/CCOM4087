let rec print_nums n foo =

  (*print_int foo;*)
  (*print_int n;*)

  if foo = n then
    print_int n
  else

    (print_int foo; print_newline () ;
    print_nums n (foo +1) );;



print_nums 5 1 ;