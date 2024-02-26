let rec factorial n =
    if n <= 1 then
      1
    else
        n * factorial (n-1);;


(*let i = Sys.argv(1);;*)
Printf.printf "%d! = %d\n" 5 (factorial 5);;