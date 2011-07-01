open Ali;;

exception Test;;

Printf.printf "Plugin Running\n";;
flush stdout;;

type cornerbook = {
  mutable wrap: int;
  mutable nuw: int;
  mutable nsw: int;
  mutable nowrap: int;
  mutable exact: int;
  mutable noexact: int
};;

type instbook = {
  mutable ret: int; 
  mutable br: int;
  mutable switch: int;
  mutable indirectBr: int;
  mutable invoke: int;
  mutable unwind: int;
  mutable unreachable: int;
  mutable binOp: int;
  mutable alloca: int;
  mutable load: int;
  mutable store: int;
  mutable getElementPtr: int;
  mutable icmp: int;
  mutable fcmp: int;
  mutable cast: int;
  mutable select: int;
  mutable phi: int;
  mutable extractValue: int;
  mutable insertValue: int;
  mutable extractElement: int;
  mutable insertElement: int;
  mutable shuffleVector: int;
  mutable va_arg: int;
  mutable call: int;
};;

type castbook = {
  mutable trunc: int; 
  mutable zext: int;
  mutable sext: int;
  mutable fpTrunc: int;
  mutable fpExt: int;
  mutable fpToUi: int;
  mutable fpToSi: int;
  mutable uiToFp: int;
  mutable siToFp: int;
  mutable ptrToInt: int;
  mutable intToPtr: int;
  mutable bitCast: int
};;

let ib = {    
   ret= 0; 
   br= 0;
   switch= 0;
   indirectBr= 0;
   invoke= 0;
   unwind= 0;
   unreachable= 0;
   binOp= 0;
   alloca= 0;
   load= 0;
   store= 0;
   getElementPtr= 0;
   icmp= 0;
   fcmp= 0;
   cast= 0;
   select= 0;
   phi= 0;
   extractValue= 0;
   insertValue= 0;
   extractElement= 0;
   insertElement= 0;
   shuffleVector= 0;
   va_arg= 0;
   call= 0;
};;

let cb = {
   trunc= 0; 
   zext= 0;
   sext= 0;
   fpTrunc= 0;
   fpExt= 0;
   fpToUi= 0;
   fpToSi= 0;
   uiToFp= 0;
   siToFp= 0;
   ptrToInt= 0;
   intToPtr= 0;
   bitCast= 0
};;

let cob = {
   wrap= 0;
   nuw= 0;
   nsw= 0;
   nowrap= 0;
   exact= 0;
   noexact= 0
};;

let reg_wrap w = 
  match w with 
    | Wnone -> cob.wrap <- cob.wrap + 1;
    | Wnsw -> cob.nsw <- cob.nsw + 1;
    | Wnuw -> cob.nuw <- cob.nuw + 1;
    | Wboth -> cob.nowrap <- cob.nowrap + 1

let reg_exact e = 
  if e 
  then cob.exact <- cob.exact + 1 
  else cob.noexact <- cob.noexact + 1

let reg_op o = 
  match o with 
    | Add x -> reg_wrap x 
    | FAdd -> ()
    | Sub x -> reg_wrap x 
    | FSub -> ()
    | Mul x -> reg_wrap x 
    | FMul -> ()
    | UDiv x -> reg_exact x 
    | SDiv x -> reg_exact x 
    | FDiv -> ()
    | URem -> ()
    | SRem -> ()
    | FRem -> ()
    | Shl x -> reg_wrap x 
    | LShr x -> reg_exact x 
    | AShr x -> reg_exact x 
    | And -> ()
    | Or -> ()
    | Xor -> ()

let reg_cb c = 
  match c with 
    | Trunc -> cb.trunc <- (+) 1 cb.trunc
    | Zext -> cb.zext <- (+) 1 cb.zext
    | Sext -> cb.sext <- (+) 1 cb.sext
    | FpTrunc -> cb.fpTrunc <- (+) 1 cb.fpTrunc
    | FpExt -> cb.fpExt <- (+) 1 cb.fpExt
    | FpToUi -> cb.fpToUi <- (+) 1 cb.fpToUi
    | FpToSi -> cb.fpToSi <- (+) 1 cb.fpToSi
    | UiToFp -> cb.uiToFp <- (+) 1 cb.uiToFp
    | SiToFp -> cb.siToFp <- (+) 1 cb.siToFp
    | PtrToInt -> cb.ptrToInt <- (+) 1 cb.ptrToInt
    | IntToPtr -> cb.intToPtr <- (+) 1 cb.intToPtr
    | BitCast -> cb.bitCast <- (+) 1 cb.bitCast
      
let reg_inst i = 
  match i with
    | Ret _ -> ib.ret <- ib.ret + 1 
    | Br _ -> ib.br <- ib.br + 1 
    | Switch _ -> ib.switch <- ib.switch + 1 
    | IndirectBr _ -> ib.indirectBr <- ib.indirectBr + 1 
    | Invoke _ -> ib.invoke <- ib.invoke + 1 
    | Unwind _ -> ib.unwind <- ib.unwind + 1 
    | Unreachable _ -> ib.unreachable <- ib.unreachable + 1 
    | BinOp (_,o,_,_,_) -> reg_op o ; ib.binOp <- ib.binOp + 1 
    | Alloca _ -> ib.alloca <- ib.alloca + 1 
    | Load _ -> ib.load <- ib.load + 1 
    | Store _ -> ib.store <- ib.store + 1 
    | GetElementPtr _ -> ib.getElementPtr <- ib.getElementPtr + 1 
    | Icmp _ -> ib.icmp <- ib.icmp + 1 
    | Fcmp _ -> ib.fcmp <- ib.fcmp + 1 
    | Cast (_,c,_,_) -> reg_cb c ; ib.cast <- ib.cast + 1 
    | Select _ -> ib.select <- ib.select + 1 
    | Phi _ -> ib.phi <- ib.phi + 1 
    | ExtractValue _ -> ib.extractValue <- ib.extractValue + 1 
    | InsertValue _ -> ib.insertValue <- ib.insertValue + 1 
    | ExtractElement _ -> ib.extractElement <- ib.extractElement + 1 
    | InsertElement _ -> ib.insertElement <- ib.insertElement + 1 
    | ShuffleVector _ -> ib.shuffleVector <- ib.shuffleVector + 1 
    | Va_arg _ -> ib.va_arg <- ib.va_arg + 1 
    | Call _ -> ib.call <- ib.call + 1 

let count (f : func) =
  List.iter (fun b-> List.iter reg_inst b.instrs) f.fbody

let print_report () = 
  Printf.printf "Instruction accounting: \n";
  Printf.printf  "=======================\n";
  Printf.printf "ret: %i\n" ib.ret;
  Printf.printf "br: %i\n" ib.br;
  Printf.printf "switch: %i\n" ib.switch;
  Printf.printf "indirectbr: %i\n" ib.indirectBr;
  Printf.printf "invoke: %i\n" ib.invoke;
  Printf.printf "unwind: %i\n" ib.unwind;
  Printf.printf "unreachable: %i\n" ib.unreachable;
  Printf.printf "binop: %i\n" ib.binOp;
  Printf.printf "alloca: %i\n" ib.alloca;
  Printf.printf "load: %i\n" ib.load;
  Printf.printf "store: %i\n" ib.store;
  Printf.printf "getelementptr: %i\n" ib.getElementPtr;
  Printf.printf "icmp: %i\n" ib.icmp;
  Printf.printf "fcmp: %i\n" ib.fcmp;
  Printf.printf "cast: %i\n" ib.cast;
  Printf.printf "select: %i\n" ib.select;
  Printf.printf "phi: %i\n" ib.phi;
  Printf.printf "extractvalue: %i\n" ib.extractValue;
  Printf.printf "insertvalue: %i\n" ib.insertValue;
  Printf.printf "extractelement: %i\n" ib.extractElement;
  Printf.printf "insertelement: %i\n" ib.insertElement;
  Printf.printf "shufflevector: %i\n" ib.shuffleVector;
  Printf.printf "va_arg: %i\n" ib.va_arg;
  Printf.printf "call: %i\n" ib.call;
  Printf.printf "-----------------------\n";
  Printf.printf "trunc: %i\n" cb.trunc;
  Printf.printf "zext: %i\n" cb.zext;
  Printf.printf "sext: %i\n" cb.sext;
  Printf.printf "fptrunc: %i\n" cb.fpTrunc;
  Printf.printf "fpext: %i\n" cb.fpExt;
  Printf.printf "fptoui: %i\n" cb.fpToUi;
  Printf.printf "fptosi: %i\n" cb.fpToSi;
  Printf.printf "uitofp: %i\n" cb.uiToFp;
  Printf.printf "sitofp: %i\n" cb.siToFp;
  Printf.printf "ptrtoint: %i\n" cb.ptrToInt;
  Printf.printf "inttoptr: %i\n" cb.intToPtr;
  Printf.printf "bitcast: %i\n" cb.bitCast;
  Printf.printf "-----------------------\n";
  Printf.printf "wrap: %i\n" cob.wrap; 
  Printf.printf "nsw: %i\n" cob.nsw; 
  Printf.printf "nuw: %i\n" cob.nuw; 
  Printf.printf "no wrap: %i\n" cob.nowrap; 
  Printf.printf "exact: %i\n" cob.exact; 
  Printf.printf "no exact: %i\n" cob.noexact; 
  Printf.printf "=======================\n"

let f (x : func) = 
  count x; print_report(); flush stdout; x
  (*print x; flush stdout; x *)
;;

register f;;


