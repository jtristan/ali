
type typ =
  | Void
  | Float
  | Double
  | X86_FP80
  | FP128
  | PPC_FP128
  | Label
  | Metadata
  | X86_MMX
  | Opaque
  | IntT of int32 
  | FunctionT of typ * typ list
  | StructT of typ list 
  | ArrayT of typ
  | PointerT of typ
  | VectorT of typ

type wrap =
  | Wnone
  | Wnsw
  | Wnuw
  | Wboth

type exact = bool

type bop = 
  | Add of wrap | FAdd | Sub of wrap | FSub
  | Mul of wrap | FMul | UDiv of exact | SDiv of exact | FDiv
  | URem | SRem | FRem
  | Shl of wrap | LShr of exact | AShr of exact | And | Or | Xor
      
type icmpOp = 
  | EQ | NE
  | UGT | UGE | ULT | ULE
  | SGT | SGE | SLT | SLE
      
type fcmpOp = 
  | Oeq | Ogt | Oge | Olt | Ole | One | ORD
  | Ueq | Ugt | Uge | Ult | Ule | Une | Uno
  
(* Missing constant expressions *)
type constant =
  | True
  | False
  | I of int64
  | F of float
  | Null
  | StructC of constant list
  | ArrayC of constant list
  | VectorC of constant list
  | ZeroInitializer
  | MetadataC of constant list
  | Glob of string
  | Fun of string
  | Undef
  | Blockaddress of string * string
  | NYI

type var = string

type operand = 
  | Const of constant
  | Var of var

type alignment = int32

type volatile = bool

type instruction = 
  | Ret of typ * operand
  | Br of operand * operand * operand
  | Switch of string
  | IndirectBr of string
  | Invoke of string
  | Unwind of string
  | Unreachable of string
  | BinOp of var * bop * typ * operand * operand
  | Alloca of var * typ * (typ * int32) option * alignment option
  | Load of var * volatile * typ * operand * alignment option  
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

type arg = {nam: string; typ: typ}
type program = {name: string; args: arg list; body: code}
type transform = program -> program

let rec print_type oc t = 
  let f = fun s -> Printf.fprintf oc "%s" s in 
  match t with
    | Void -> f "void"
    | Float -> f "float"
    | Double -> f "double"
    | X86_FP80 -> f "x86_fp80"
    | FP128 -> f "fp128"
    | PPC_FP128 -> f "ppc_fp128"
    | X86_MMX -> f "x86_mmx"
    | Label -> f "label"
    | Metadata -> f "Metadata"
    | IntT width -> f ("i"^Int32.to_string width) 
    | ArrayT t -> Printf.fprintf oc "[%a]" print_type t
    | PointerT t -> Printf.fprintf oc "%a*" print_type t
    | VectorT t -> Printf.fprintf oc "<%a>" print_type t
    | _ -> f "Type WTF?"
  
let print_constant oc c = 
  match c with
    | I i -> Printf.fprintf oc "%s " (Int64.to_string i)
    | _ -> Printf.fprintf oc "NYI"

let print_operand oc o = 
  match o with
    | Const c ->  Printf.fprintf oc "%a" print_constant c ; flush stdout
    | Var v ->  Printf.fprintf oc "%s" v

let print_bop oc b = 
  let s = 
    match b with 
      | Add _ -> "add"
      | FAdd -> "fadd"
      | Sub _ -> "sub"
      | FSub -> "fsub"
      | Mul _ -> "mul"
      | FMul -> "fmul"
      | UDiv _ -> "udiv"
      | SDiv _ -> "sdiv"
      | FDiv -> "fdiv"
      | URem ->  "urem"
      | SRem -> "srem"
      | FRem -> "frem"
      | Shl _ -> "shl"
      | LShr _ -> "lshr"
      | AShr _ -> "ashr"
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
  in
  Printf.fprintf oc "%s" s; flush stdout
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
    | BinOp (dst,o,t,e1,e2) -> Printf.fprintf oc "%s = %a %a %a, %a" dst print_bop o print_type t print_operand e1 print_operand e2
    | Alloca _ -> Printf.fprintf oc "Alloca"
    | Load _ -> Printf.fprintf oc "Load"
    | Store _ -> Printf.fprintf oc "Store"
    | GetElelemtPtr _ -> Printf.fprintf oc "GetElementPtr"
    | _ -> Printf.fprintf oc "Instruction NYI\n"
;;

let print_basicBlock oc b = 
  Printf.fprintf oc "\n";
  Printf.fprintf oc "%s\n" b.label;
  List.iter (fun i -> print_instruction oc i; Printf.fprintf oc "\n"; flush stdout) b.instrs  

let print_body oc =
  List.iter (print_basicBlock oc) 

let print_args oc args = 
  List.iter (fun arg -> Printf.fprintf oc "%s: %a; " arg.nam print_type arg.typ) args

let print_function oc f = 
  Printf.fprintf oc "{name: %s;\n args: %a;\n body: %a\n}" f.name print_args f.args print_body f.body 

let print = print_function stdout

let register (f : transform) = Callback.register "transform" f

