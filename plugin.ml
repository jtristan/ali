open Dynlink;;
open Callback;;

let path = 
  try
    Sys.getenv "ALI"
  with 
    | Not_found -> failwith "You must setup the ALI environment vairable"
;;

let error_msg msg = 
  failwith ("Loading of OCaml plugin failed: " ^ msg)
;;

let decypher_link_error e =
  match e with
    | Undefined_global em -> " undefined global " ^ em
    | Unavailable_primitive em -> " unavailable primitive " ^ em
    | Uninitialized_global em -> " unitialized global " ^ em


let decypher_error e = 
  match e with
    | Not_a_bytecode_file em -> "not a bytecode file " ^ em
    | Inconsistent_import em -> "inconsistent import " ^ em
    | Unavailable_unit em -> "unavailable unit " ^ em
    | Unsafe_file -> "unsafe_file"
    | Linking_error (em,le) -> "linking error " ^ em ^ (decypher_link_error le)
    | Corrupted_interface em -> "corrupted interface " ^ em
    | File_not_found em -> "file not found " ^ em
    | Cannot_open_dll em -> "cannot open dll " ^ em
    | Inconsistent_implementation em -> "inconsistent implementation " ^ em
;;

allow_unsafe_modules true;;

try 
  Printf.printf "Loading the standard library\n";
  loadfile "/usr/local/lib/ocaml/stdlib.cma";
  Printf.printf "Loading the academic LLVM interface/n";
  loadfile "/Users/jean-baptistetristan/Code/llvm-2.9/lib/Transforms/ali/ali.cmo";
  Printf.printf "Loading your plugin/n";
  loadfile path
with
  | Error e -> error_msg (decypher_error e)
;;



