
type bop = 
  | Add | FAdd | Sub | FSub
  | Mul | FMul | UDiv | SDiv | FDiv
  | URem | SRem | FRem
  | Shl | LShr | AShr | And | Or | Xor
      
type icmpOp = 
  | EQ | NE
  | UGT | UGE | ULT | ULE
  | SGT | SGE | SLT | SLE
      
type fcmpOp = 
  | Oeq | Ogt | Oge | Olt | Ole | One | ORD
  | Ueq | Ugt | Uge | Ult | Ule | Une | Uno
  
type constant =
  | Int
  | FP
  | NYI

type var = string

type instruction = 
  | Ret of string 
  | Br of string
  | Switch of string
  | IndirectBr of string
  | Invoke of string
  | Unwind of string
  | Unreachable of string
  | BinOp of bop * string * string
  | Alloca of string
  | Load of string
  | Store of string
  | GetElelemtPtr of string
  | CastOp of string
  | Icmp of icmpOp 
  | Fcmp of fcmpOp
  | Phi of string
  | Call of string
  | Select of string
  | NYI
   
type basicBlock = {label: string; instrs: instruction list}
type code = basicBlock list

type arg = {nam: string; typ: int}
type program = {name: string; args: arg list; body: code}
type transform = program -> program

let print_bop oc b = 
  let s = 
    match b with 
      | Add -> " + "
      | FAdd -> " + "
      | Sub -> " - "
      | FSub -> " - "
      | Mul -> " * "
      | FMul -> " * "
      | UDiv -> " / "
      | SDiv -> " / "
      | FDiv -> " / "
      | URem ->  " % "
      | SRem -> " % "
      | FRem -> " % "
      | Shl -> " << "
      | LShr -> " >> "
      | AShr -> " >> "
      | And -> " & "
      | Or -> " | "
      | Xor -> " ^ "
  in
  Printf.fprintf oc "%s" s
;;

let print_instruction oc i =
  match i with
    | Ret _ -> Printf.fprintf oc "Ret"
    | Br _ -> Printf.fprintf oc "Br"
    | Switch _ -> Printf.fprintf oc "Switch"
    | IndirectBr _ -> Printf.fprintf oc "IndirectBr"
    | Invoke _ -> Printf.fprintf oc "Invoke"
    | Unwind _ -> Printf.fprintf oc "Unwind"
    | Unreachable _ -> Printf.fprintf oc "Unreachable"
    | BinOp (o,e1,e2) -> Printf.fprintf oc "%s %a %s" "e1" print_bop o "e2"
    | Alloca _ -> Printf.fprintf oc "Alloca"
    | Load _ -> Printf.fprintf oc "Load"
    | Store _ -> Printf.fprintf oc "Store"
    | GetElelemtPtr _ -> Printf.fprintf oc "GetElementPtr"
    | _ -> Printf.fprintf oc "Instruction NYI\n"
;;

let print_basicBlock oc b = 
  Printf.fprintf oc "\n";
  Printf.fprintf oc "%s\n" b.label;
  List.iter (fun i -> print_instruction oc i; Printf.fprintf oc "\n") b.instrs  

let print_body oc =
  List.iter (print_basicBlock oc) 

let print_args oc args = 
  List.iter (fun arg -> Printf.fprintf oc "%s: %s; " arg.nam "type") args

let print_function oc f = 
  Printf.fprintf oc "{name: %s;\n args: %a;\n body: %a\n}" f.name print_args f.args print_body f.body 

let print = print_function stdout

let register (f : transform) = Callback.register "transform" f

