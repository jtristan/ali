(***********************************************************************)
(*                                                                     *)
(*                               ALI                                   *)
(*                                                                     *)
(*                       Jean-Baptiste Tristan                         *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  described in file ../../LICENSE.                                   *)
(*                                                                     *)
(***********************************************************************)

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
  | ArrayT of int32 * typ (* int64 in LLVM *)
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
  ginfo: info;
  gspace: string option
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

type analysis = modul -> unit

open Gc

exception Caml

let _ = Callback.register_exception "camlexn" (Caml)

let wrap f = 
  fun x ->
    try f x with 
      | _ -> raise Caml 

let register (f : analysis) = Callback.register "analysis" (wrap f)

let set () = 
  Gc.set { (Gc.get()) with Gc.minor_heap_size = 1000000 }
;;

let _ = Callback.register "clean" Gc.minor
let _ = Callback.register "set" set

