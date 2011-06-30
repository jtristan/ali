open Ali;;

exception Test;;

Printf.printf "Plugin Running\n";;
flush stdout;;

let f (x : func) = 
  print x; flush stdout; x 
;;

register f;;


