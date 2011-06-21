open Ali;;

Printf.printf "Plugin Running\n";;
flush stdout;;

let f (x : program) = 
  print x; flush stdout; x
;;

register f;;


