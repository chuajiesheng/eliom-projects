let rec fact = function
        | 0 -> 1
        | n -> n * fact(n - 1);;

let rec read l =   
  	match l with
  	| a :: b -> fact(a) + read(b)          
  	| [] -> 0;;

let a = [1; 2; 3; 4; 5];;

let b = fact(5);;
let c = read(a);;

print_int b;;
print_int c;;
