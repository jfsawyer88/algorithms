(** FizzBuzz Challenge **)

let rec loop i =
  if i <= 100 then
    if 0 = i mod 3 then
      if 0 = i mod 5 then
	print_string "FizzBuzz"
      else
	print_string "Fizz"
    else
      if 0 = i mod 5 then
	print_string "Buzz"
      else
	print_int i;
  print_newline ();
  loop (i + 1)
;;

(** does not work, find out why**)
(*loop 1;;*)

(** imperative? **)
for i = 1 to 100 do
  if 0 = i mod 3 then
    if 0 = i mod 5 then
      print_string "FizzBuzz"
    else
      print_string "Fizz"
  else
    if 0 = i mod 5 then
      print_string "Buzz"
    else
      print_int i;
  print_newline()
done

