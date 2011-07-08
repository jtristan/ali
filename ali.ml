

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
  | ArrayT of int32 * typ
  | PointerT of typ
  | VectorT of typ
  | Rec of int32
  | Named of string

type wrap =
  | Wnone
  | Wnsw
  | Wnuw
  | Wboth

type exact = bool

type bop = 
  | Add of wrap 
  | FAdd 
  | Sub of wrap 
  | FSub
  | Mul of wrap 
  | FMul 
  | UDiv of exact 
  | SDiv of exact 
  | FDiv
  | URem 
  | SRem 
  | FRem
  | Shl of wrap 
  | LShr of exact 
  | AShr of exact 
  | And 
  | Or 
  | Xor
      
type icmpOp = 
  | EQ 
  | NE
  | UGT 
  | UGE 
  | ULT 
  | ULE
  | SGT 
  | SGE 
  | SLT 
  | SLE
      
type fcmpOp = 
  | Ffalse 
  | Oeq 
  | Ogt 
  | Oge 
  | Olt 
  | Ole 
  | One 
  | Ord
  | Uno 
  | Ueq 
  | Ugt 
  | Uge 
  | Ult 
  | Ule 
  | Une 
  | Ftrue
  
type castop =
  | Trunc 
  | Zext
  | Sext
  | FpTrunc
  | FpExt
  | FpToUi
  | FpToSi
  | UiToFp
  | SiToFp
  | PtrToInt
  | IntToPtr
  | BitCast

type inbound = bool

type index = int32

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
  | GetElementPtrC of constant * constant list
  | CastC of castop * typ * constant * typ

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

type fattribute =    
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

type label = string

type top = typ * operand

type tail = bool

type instruction = 
  | Ret of top option
  | Br of top option * label * label option
  | Switch of top * label * (top * label) list
  | IndirectBr of top * label list
  | Invoke of var * calling_convention * attribute list * top * top list * fattribute list * label * label
  | Unwind
  | Unreachable
  | BinOp of var * bop * typ * top * top
  | Alloca of var * typ * (typ * int32) option * alignment
  | Load of var * volatile * top * alignment
  | Store of volatile * top * top * alignment
  | GetElementPtr of var * inbound * top * top list 
  | Cast of var * castop * top * typ
  | Icmp of var * icmpOp * typ * top * top
  | Fcmp of var * fcmpOp * typ * top * top
  | Phi of var * typ * (label * top) list
  | Select of var * top * top * top 
  | ExtractElement of var * top * index
  | InsertElement of var * top * top * index
  | ShuffleVector of var * top * top * top
  | ExtractValue of var * top * index list
  | InsertValue of var * top * top * index list
  | Call of var * tail * calling_convention * attribute list * typ * top * top list * fattribute list
  | Va_arg of string
   
type basicBlock = {label: string; instrs: instruction list}
type code = basicBlock list

type linkage = 
  | Private
  | Linker_private
  | Linker_private_weak
  | Linker_private_weak_def_auto
  | Internal
  | Available_externally
  | Linkonce
  | Weak
  | Common
  | Appending
  | Extern_weak
  | Linkonce_odr
  | Weak_odr
  | Externally_visible
  | Dllimport
  | Dllexport

type visibility =
  | Default
  | Hidden
  | Protected

type gc = string

type arg = {
  nam: string; 
  typ: typ
}

type info = {
  linkage: linkage;
  visibility: visibility;
  alignment: alignment;
  section: string
}

type alias = { 
  aname: string;
  aaliaseeType: typ;
  aaliasee: var;
  ainfo: info
}

type func = {
  fname: string; 
  fcconv: calling_convention option;
  fretat: attribute list;
  frettype: typ;
  fargs: arg list;
  ffattr: fattribute list;
  fgc: gc option;
  fbody: code;
  finfo: info
}

type global = {
  gname: string;
  gtyp: typ;
  ginit: constant option;
  gthread_local: bool;
  gconstant: bool;
  ginfo: info
}

type namedtype = {
  tname: string;
  ttype: typ
} 
    
type modul = {
  midentifier: string;
  mdatalayout: string;
  mtargettriple: string;
  mglobals: global list;
  mfunctions: func list;
  malias: alias list;
  mlibraries: string list;
  mtypenames: namedtype list
}

type transform = modul -> modul

let rec print_list printer oc l = 
  match l with
    | [] -> ()
    | x :: [] -> Printf.fprintf oc " %a " printer x
    | x :: l -> Printf.fprintf oc " %a," printer x; print_list printer oc l
;;

let rec print_list_2 printer oc l =
  match l with
    | [] -> ()
    | x :: [] -> Printf.fprintf oc "%a" printer x
    | x :: l -> Printf.fprintf oc "%a, " printer x; print_list printer oc l
;;  

let rec print_type oc t = 
  flush stdout;
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
    | ArrayT (i,t) -> Printf.fprintf oc "[%s x %a]" (Int32.to_string i) print_type t
    | PointerT t -> Printf.fprintf oc "%a*" print_type t
    | VectorT t -> Printf.fprintf oc "<%a>" print_type t
    | StructT t -> Printf.fprintf oc "{%a}" (print_list print_type) t 
    | Opaque -> Printf.fprintf oc "opaque"
    | FunctionT (r,args) -> Printf.fprintf oc "%a (%a)" print_type r (print_list print_type) args
    | Rec i -> Printf.fprintf oc "rec %s" (Int32.to_string i) 
    | Named s -> Printf.fprintf oc "%s" ("%"^s)

let print_castOp oc o = 
  let s = 
    match o with
      | Trunc -> "trunc"
      | Zext -> "zext"
      | Sext -> "sext"
      | FpTrunc -> "fptruc"
      | FpExt -> "fpext"
      | FpToUi -> "fptoui"
      | FpToSi -> "fptosi"
      | UiToFp -> "uitofp"
      | SiToFp -> "sitofp"
      | PtrToInt -> "ptrtotint"
      | IntToPtr -> "inttoptr"
      | BitCast -> "bitcast"
  in 
  Printf.fprintf oc "%s" s

let rec print_constant oc c = 
  flush stdout;
  match c with
    | True -> Printf.fprintf oc "true"
    | False -> Printf.fprintf oc "false"
    | I i -> Printf.fprintf oc "%s " (Int64.to_string i)
    | F f -> Printf.fprintf oc "%f " f
    | Null -> Printf.fprintf oc "null"
    | StructC s -> Printf.fprintf oc "{%a}" (print_list print_constant) s
    | ArrayC a -> Printf.fprintf oc "[%a]" (print_list print_constant) a
    | VectorC _ -> Printf.fprintf oc "Implement vector constant"
    | ZeroInitializer -> Printf.fprintf oc "zeroinitialiser"
    | MetadataC _ -> Printf.fprintf oc "Implement metadata constant"
    | Glob s -> Printf.fprintf oc "@%s" s
    | Fun s -> Printf.fprintf oc "@%s " s
    | Undef -> Printf.fprintf oc "undef"
    | Blockaddress (s1,s2) -> Printf.fprintf oc "Implement blockaddress"
    | GetElementPtrC (c,l) -> Printf.fprintf oc "getelementptr %a %a" print_constant c (print_list print_constant) l 
    | CastC (c,tsrc,o,tdst) -> Printf.fprintf oc "cast" 

let pr s = "%"^s

let print_operand oc o = 
  match o with
    | Const c ->  Printf.fprintf oc "%a" print_constant c 
    | Var v ->  Printf.fprintf oc "%s" (pr v)

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

let print_icmpOp oc o =
  let s = 
    match o with
      | EQ -> "eq"
      | NE -> "ne"
      | UGT -> "ugt"
      | UGE -> "uge"
      | ULT -> "ult"
      | ULE -> "ule"
      | SGT -> "sgt"
      | SGE -> "sge"
      | SLT -> "slt"
      | SLE -> "sle"
  in
  Printf.fprintf oc "%s" s
;;
      
let print_fcmpOp oc o =
  let s = 
    match o with
      | Ffalse -> "false"
      | Oeq -> "oeq"
      | Ogt -> "ogt"
      | Oge -> "oge"
      | Olt -> "olt"
      | Ole -> "ole"
      | One -> "one"
      | Ord -> "ord"
      | Uno -> "uno"
      | Ueq -> "ueq"
      | Ugt -> "ugt"
      | Uge -> "uge"
      | Ult -> "ult"
      | Ule -> "ule"
      | Une -> "une"
      | Ftrue -> "true"
  in
  Printf.fprintf oc "%s" s



let print_align oc a = 
  match Int32.to_int a with
    | 0 -> Printf.fprintf oc ""
    | _ -> Printf.fprintf oc ", align %s" (Int32.to_string a)

let string_volatile v = if v then "volatile " else ""

let string_inbounds i = if i then "inbounds" else ""

let print_top oc t = 
  flush stdout;
  Printf.fprintf oc "%a %a"  print_type (fst t) print_operand (snd t)
;;

let print_ret oc r =
  Printf.fprintf oc "return";
  match r with
    | None -> ()
    | Some t -> Printf.fprintf oc " %a" print_top t
;;

let print_option printer oc o =
  match o with 
    | None -> ()
    | Some x -> printer oc x

let print_label oc l =
  Printf.fprintf oc "%s" l

let print_instruction oc i =
  Printf.fprintf oc " "; flush stdout;
  match i with
    | Ret r -> Printf.fprintf oc "ret %a" (print_option print_top) r
    | Br (cond,l1,l2) -> Printf.fprintf oc "br %a %s %a" (print_option print_top) cond l1 (print_option print_label) l2  
    | Switch _ -> Printf.fprintf oc "Switch"
    | IndirectBr _ -> Printf.fprintf oc "IndirectBr"
    | Invoke _ -> Printf.fprintf oc "Invoke"
    | Unwind _ -> Printf.fprintf oc "Unwind"
    | Unreachable _ -> Printf.fprintf oc "Unreachable"
    | BinOp (dst,o,t,e1,e2) -> Printf.fprintf oc "%s = %a %a %a, %a" (pr dst) print_bop o print_type t print_top e1 print_top e2
    | Alloca (dst,t,_,al) -> Printf.fprintf oc "%s = alloca %a%a" (pr dst) print_type t print_align al
    | Load (dst,vol,o,al) -> Printf.fprintf oc "%s = %sload %a%a" (pr dst) (string_volatile vol) print_top o print_align al
    | Store (vol,e1,e2,al) -> Printf.fprintf oc "%sstore %a, %a%a" (string_volatile vol) print_top e1 print_top e2 print_align al
    | GetElementPtr (dst,b,e,idx) -> Printf.fprintf oc "%s = getelementptr %s %a, %a" (pr dst) (string_inbounds b) print_top e (print_list print_top) idx
    | Icmp (dst,c,_,e1,e2) -> Printf.fprintf oc "%s = icmp %a %a, %a" (pr dst) print_icmpOp c  print_top e1 print_top e2
    | Fcmp (dst,c,_,e1,e2) -> Printf.fprintf oc "%s = fcmp %a %a, %a" (pr dst) print_fcmpOp c  print_top e1 print_top e2
    | Cast (dst,op,e,t) -> Printf.printf "%s = %a %a to %a" (pr dst) print_castOp op print_top e print_type t 
    | Select _ -> Printf.fprintf oc "select"
    | Phi _ -> Printf.fprintf oc "phi"
    | ExtractValue _ -> Printf.fprintf oc "extractvalue"
    | InsertValue _ -> Printf.fprintf oc "insertvalue"
    | ExtractElement _ -> Printf.fprintf oc "extractelement"
    | InsertElement _ -> Printf.fprintf oc "insertelement"
    | ShuffleVector _ -> Printf.fprintf oc "shufflevector"
    | Va_arg _ -> Printf.fprintf oc "va_arg"
    | Call (dst,_,_,_,retyp,f,args,_) -> Printf.fprintf oc "%s = call %a %a (%a)" (pr dst) print_type retyp print_top f (print_list_2 print_top) args
;;

let print_linkage oc l = 
  flush stdout;
  let s = 
    match l with
      | Private -> "private"
      | Linker_private -> "linker_private"
      | Linker_private_weak -> "linker_private_weak"
      | Linker_private_weak_def_auto -> "linker_private_weak_def_auto"
      | Internal -> "internal"
      | Available_externally -> "available_externally"
      | Linkonce -> "linkonce"
      | Weak -> "weak"
      | Common -> "common"
      | Appending -> "appending"
      | Extern_weak -> "extern_weak"
      | Linkonce_odr -> "linkonce_odr"
      | Weak_odr -> "weal_odr"
      | Externally_visible -> ""
      | Dllimport -> "dllimport"
      | Dllexport -> "dllexport"
  in
  Printf.fprintf oc "%s" s

let print_visibility oc v =
  flush stdout;
  let s = 
    match v with
      | Default -> "" 
      | Hidden -> "hidden"
      | Protected -> "protected"
  in
  Printf.fprintf oc "%s" s

let print_basicBlock oc b = 
  Printf.fprintf oc "; <label>:%s\n" b.label; flush stdout;
  List.iter (fun i -> print_instruction oc i; Printf.fprintf oc "\n"; flush stdout) b.instrs;
  Printf.fprintf oc "\n"

let print_body oc =
  List.iter (print_basicBlock oc) 
;;

let print_arg oc arg = 
  flush stdout; 
  Printf.fprintf oc "%a %s" print_type arg.typ arg.nam
;;

let print_formal_params oc args = 
  flush stdout;
  (print_list_2 print_arg) oc args 
;;

open Gc

let print_function oc (f: func) =
  flush stdout;
  Printf.fprintf oc "define @%s (%a) {\n%a}\n" f.fname print_formal_params f.fargs print_body f.fbody

let print_named_type oc nt = 
  Printf.fprintf oc "%s = type %a\n" ("%"^nt.tname) print_type nt.ttype; flush stdout  

let is_string t = 
  match t with
    | ArrayT (_,IntT i) -> Int32.to_int i = 8
    | _ -> false

let itc i = 
  flush stdout;
  match i with
    | I i -> char_of_int (Int64.to_int i)
    | _ -> failwith "false assmption for itc\n"

let print_constant_as_string oc l = 
  match l with 
    | Some (ArrayC l) -> List.iter (fun x -> Printf.fprintf oc "%c" (itc x)) l
    | _ -> failwith "print_constant_as_string false assumption\n"

let string_alignment a = 
  if Int32.to_int a = 0 then "" else ", align "^ (Int32.to_string a) 

let string_section s = 
  if s = "" then "" else ", section \"s\""

let print_global oc g = 
  Printf.fprintf oc "@%s = %a %a %s %a %a%s%s\n" 
    g.gname 
    print_visibility g.ginfo.visibility
    print_linkage g.ginfo.linkage
    (if g.gconstant then "constant" else "global") 
    print_type g.gtyp 
    (fun oc init -> if is_string g.gtyp 
     then (print_option print_constant) oc init (* (print_constant_as_string oc init) *)
     else (print_option print_constant) oc init
    ) g.ginit
    (string_section g.ginfo.section)
    (string_alignment g.ginfo.alignment)
  ; flush stdout

let print_module oc (m: modul) = 
  Printf.fprintf oc "; ModuleID = '%s'\n" m.midentifier; flush stdout;
  Printf.fprintf oc "\"target datalayout = %s\"\n" m.mdatalayout; flush stdout;
  Printf.fprintf oc "\"target triple = %s\"\n\n" m.mtargettriple; flush stdout;
  List.iter (print_named_type oc) m.mtypenames; flush stdout ;
  Printf.fprintf oc "\n";
  List.iter (print_global oc) m.mglobals; flush stdout; 
  List.iter (print_function oc) m.mfunctions 

let print = 
  print_module stdout

exception Caml
let _ = Callback.register_exception "camlexn" (Caml)

let wrap f = 
  fun x ->
    try f x with 
      | _ -> raise Caml 

let register (f : transform) = Callback.register "transform" (wrap f)

let set () = 
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 1000000000 }
;;

let _ = Callback.register "clean" Gc.minor
let _ = Callback.register "set" set

