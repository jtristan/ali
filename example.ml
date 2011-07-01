open Ali;;

exception Test;;

Printf.printf "Plugin Running\n";;
flush stdout;;

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

let reg_inst i = 
  match i with
    | Ret _ -> ib.ret <- ib.ret + 1 
    | Br _ -> ib.br <- ib.br + 1 
    | Switch _ -> ib.switch <- ib.switch + 1 
    | IndirectBr _ -> ib.indirectBr <- ib.indirectBr + 1 
    | Invoke _ -> ib.invoke <- ib.invoke + 1 
    | Unwind _ -> ib.unwind <- ib.unwind + 1 
    | Unreachable _ -> ib.unreachable <- ib.unreachable + 1 
    | BinOp _ -> ib.binOp <- ib.binOp + 1 
    | Alloca _ -> ib.alloca <- ib.alloca + 1 
    | Load _ -> ib.load <- ib.load + 1 
    | Store _ -> ib.store <- ib.store + 1 
    | GetElementPtr _ -> ib.getElementPtr <- ib.getElementPtr + 1 
    | Icmp _ -> ib.icmp <- ib.icmp + 1 
    | Fcmp _ -> ib.fcmp <- ib.fcmp + 1 
    | Cast _ -> ib.cast <- ib.cast + 1 
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
  Printf.printf "=======================\n"

let f (x : func) = 
  count x; print_report(); flush stdout; x
  (*print x; flush stdout; x *)
;;

register f;;


