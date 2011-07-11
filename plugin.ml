open Dynlink;;
open Callback;;

let transform = 
  try
    Sys.getenv "TRANSFORM"
  with 
    | Not_found -> failwith "You must set up the TRANSFORM environment variable"
;;

let stdlib = 
  try 
    Sys.getenv "OCAMLSTDLIB"
  with
    | Not_found -> failwith "You must set up the OCAMLSTDLIB environment variable"
;;

let alilib = 
  try 
    Sys.getenv "ALILIB"
  with
    | Not_found -> failwith "You must set up the alilib environment variable"
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
  loadfile stdlib;
  Printf.printf "Loading the academic LLVM interface\n";
  loadfile alilib;
  Printf.printf "Loading your plugin\n";
  loadfile transform
with
  | Error e -> error_msg (decypher_error e)
;;



