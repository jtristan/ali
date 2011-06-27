
type 'a option = 
  | None
  | Some of 'a

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
  
type castop =
  | Trunc 
  | Zext
  | Sect
  | FpTrunc
  | fpExt
  | FpToUi
  | FpToSi
  | UiToFp
  | SiToFp
  | PtrToInt
  | IntToPtr
  | BitCast

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

type calling_convention =
  | C
  | Fast
  | Cold

type attribute = 
  | Zeroext
  | Signext
  | Inreg
  | Byval
  | Sret
  | Noalias
  | Nocapture
  | Nest

type fattributes =    
  | Alignstack of int32
  | Alwaysinline
  | Hotpatch
  | Nonlazybind
  | Inlinehint
  | Naked
  | Noimplicitfloat
  | Noinline
  | Noredzone
  | Noreturn
  | Nounwind
  | Optsize
  | Readnone
  | Readonly
  | Ssp
  | Sspreq

type intrinsic

type index = int32

type label = string

type top = typ * operand

type instruction = 
  | Ret of top option
  | Br of top option * label * label option
  | Switch of top * label * (top * label) list
  | IndirectBr of top * label list
  | Invoke of var * calling_convention * attribute * top * (top) list * fattribute * label * label
  | Unwind
  | Unreachable
  | BinOp of var * bop * typ * top * top
  | Alloca of var * typ * (typ * int32) option * alignment option
  | Load of var * volatile * top * alignment option  
  | Store of volatile * top * top * alignment option
      (* TODO *)
  | GetElelemtPtr of string
  | CastOp of var * castop * top * typ
  | Icmp of var * icmpOp * typ * top * top
  | Fcmp of var * fcmpOp * typ * top * top
  | Phi of var * typ * top list
  | Select of var * typ * top * top * operand
  | ExtractElement of var * top * index
  | InsertElement of var * top * top * index
  | ShuffleVector of var * top * top * top
  | ExtractValue of var * top * index * index list
  | InsertValue of var * top * top * index * index list
      (* TODO *)
  | Call of string
  | Va_arg of string
  | Intrinsic of intrinsic
   
type basicBlock = {label: string; instrs: instruction list}
type code = basicBlock list

type arg = {nam: string; typ: typ}
type func = {name: string; args: arg list; body: code}
type transform = func -> func

let rec print_type oc t = 
  let f = fun s -> Printf.fprintf oc "%s" s; flush stdout in 
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
    | Const c ->  Printf.fprintf oc "%a" print_constant c 
    | Var v ->  Printf.fprintf oc "%s" v

let string_wrap w = 
  match w with
    | Wnone -> ""
    | Wnsw -> "nsw"
    | Wnuw -> "nuw"
    | Wboth -> "nuw nsw"

let string_exact b = if b then "exact" else ""

let print_bop oc b = 
  let s = 
    match b with 
      | Add x -> "add "^string_wrap x 
      | FAdd -> "fadd"
      | Sub x -> "sub "^string_wrap x 
      | FSub -> "fsub"
      | Mul x -> "mul "^string_wrap x 
      | FMul -> "fmul"
      | UDiv x -> "udiv "^string_exact x
      | SDiv x -> "sdiv "^string_exact x
      | FDiv -> "fdiv"
      | URem ->  "urem"
      | SRem -> "srem"
      | FRem -> "frem"
      | Shl x -> "shl "^string_wrap x 
      | LShr x -> "lshr "^string_exact x
      | AShr x -> "ashr "^string_exact x
      | And -> "and"
      | Or -> "or"
      | Xor -> "xor"
  in
  Printf.fprintf oc "%s" s; flush stdout
;;

let print_align oc a = 
  match a with
    | None -> Printf.fprintf oc ""
    | Some a -> Printf.fprintf oc ", align %s" (Int32.to_string a)

let string_volatile v = if v then "volatile " else ""

let print_ret oc r =
  Printf.fprintf oc "Ret"; flush stdout;
  match r with
    | None -> ()
    | Some (t,e) -> Printf.fprintf oc " %a %a" print_type t print_operand e

let print_top oc t = 
  Printf.fprintf oc " [%a: %a]" print_type (fst t) print_operand (snd t)

let print_instruction oc i =
  match i with
    | Ret r -> print_ret oc r
    | Br _ -> Printf.fprintf oc "Br"
    | Switch _ -> Printf.fprintf oc "Switch"
    | IndirectBr _ -> Printf.fprintf oc "IndirectBr"
    | Invoke _ -> Printf.fprintf oc "Invoke"
    | Unwind _ -> Printf.fprintf oc "Unwind"
    | Unreachable _ -> Printf.fprintf oc "Unreachable"
    | BinOp (dst,o,t,e1,e2) -> Printf.fprintf oc "%s = %a %a %a, %a" dst print_bop o print_type t print_top e1 print_top e2
    | Alloca (dst,t,_,al) -> Printf.fprintf oc "%s = alloca %a %a" dst print_type t print_align al
    | Load (dst,vol,o,al) -> Printf.fprintf oc "%s = %sload %a %a" dst (string_volatile vol) print_top o print_align al
    | Store (vol,e1,e2,al) -> Printf.fprintf oc "%sstore %a, %a %a" (string_volatile vol) print_top e1 print_top e2 print_align al
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

