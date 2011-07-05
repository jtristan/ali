#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Constants.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"
#include "llvm/InstrTypes.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Module.h"
#include "llvm/TypeSymbolTable.h"

#include <map>
#include <sstream>
#include <exception>
#include <list>

extern "C"{
#include </usr/local/lib/ocaml/caml/mlvalues.h>
#include </usr/local/lib/ocaml/caml/alloc.h>
#include </usr/local/lib/ocaml/caml/callback.h>
#include </usr/local/lib/ocaml/caml/memory.h>
}

using namespace llvm;

namespace {

  class Ali : public ModulePass {
  public:
    static char ID;
    Ali() : ModulePass(ID) {
      char* t[1] = {"crap"}; 
      caml_startup(t);         
    }
    
    virtual bool runOnModule(Module &F);
    
  };

  template <class T> class Namer {
  private:
    unsigned counter;
    std::map<T,std::string> m; 
  public:
    Namer() : counter(0) {};
    void assign(T t) { 
      std::string var;
      std::ostringstream out;
      out << "%" << ++counter;
      var = out.str();
      m[t] = var; }
    std::string get(T t) { return m[t]; }
    void clear() { counter = 0; m.clear(); }
    void reg(T t, std::string name) { m[t] = name; } 
  };

  Namer<const Instruction*> instNames;
  Namer<const BasicBlock*> blockNames;

//   template <class T, class IT> value convertList(const iplist<T> *L) {
//     CAMLparam0();
//     CAMLlocal4(l,tmp,cell,a);
    
//     l = Val_int(0);
//     tmp = Val_int(0);
//     for (IT I = L->begin(), E = L->end(); I != E; ++I) {
//       cell = caml_alloc(2,0);
//       a = convert(I); 
//       Store_field(cell,0,a);
//       Store_field(cell,1,Val_int(0));
//       if (l == Val_int(0)) l = cell;
//       if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
//       tmp = cell;
//     }
    
//     CAMLreturn(l);
//   }  

  template <typename IT> value convertIT(IT begin,IT end) {
    CAMLparam0();
    CAMLlocal4(l,tmp,cell,a);
    
    l = Val_int(0);
    tmp = Val_int(0);
    for (IT I = begin; I != end; ++I) {
      cell = caml_alloc(2,0);
      a = convert(I); 
      Store_field(cell,0,a);
      Store_field(cell,1,Val_int(0));
      if (l == Val_int(0)) l = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    
    CAMLreturn(l);
  }  

  bool stop_mode = false;

  typedef std::map<const Type *,int> tMap;

  const Module * Maccess;

  // This global variable is used to properly compute the named types
  // Eventually, it must be removed
  std::string converted = "";

  value convert_aux(const Type *T, int depth, tMap *typeMap) {
    CAMLparam0();
    CAMLlocal5(typ,arg,head,current,cell);
    CAMLlocal1(tmp);

    typ = Val_int(0);

    if (Maccess->getTypeName(T) != "" && Maccess->getTypeName(T) != converted) {
      typ = caml_alloc(1,7);
      Store_field(typ,0,caml_copy_string(Maccess->getTypeName(T).c_str()));
    }
    else {
      // TRICKY
      if (typeMap->find(T) != typeMap->end() && (*typeMap)[T] != -1) {
	if (Maccess->getTypeName(T) == "") {
	  typ = caml_alloc(1,6);
	  Store_field(typ,0,caml_copy_int32((*typeMap)[T]));
	}
	else { //type refering to itself, but not trivially
	  typ = caml_alloc(1,7);
	  Store_field(typ,0,caml_copy_string(Maccess->getTypeName(T).c_str()));
	}
      }
      else {
	if (T->isPrimitiveType()) typ = Val_int(T->getTypeID());
	if (T->isIntegerTy()) {
	  typ = caml_alloc(1,0);
	  int width = cast<IntegerType>(T)->getBitWidth();
	  Store_field(typ,0,caml_copy_int32(width));
	}
	if (isa<SequentialType>(T)) {
	  (*typeMap)[T] = depth;
	  typ = caml_alloc(2,T->getTypeID() - 9); 
	  arg = convert_aux(cast<SequentialType>(T)->getElementType(),depth+1,typeMap);
	  Store_field(typ,0,arg);
	  (*typeMap)[T] = -1;
	}
	if (T->isStructTy()) {
	  (*typeMap)[T] = depth;
	  typ = caml_alloc(1,2);
	  head = Val_int(0);
	  current = Val_int(0);
	  
	  for (unsigned i = 0; i < cast<StructType>(T)->getNumElements(); ++i) {
	    cell = caml_alloc(2,0);
	    tmp = convert_aux(cast<StructType>(T)->getElementType(i),depth + 1,typeMap);
	    Store_field(cell,0,tmp);
	    Store_field(cell,1,Val_int(0));
	    if (head == Val_int(0)) head = cell;
	    if (current != Val_int(0)) Store_field(current,1,cell); 
	    current = cell;
	  }
	  Store_field(typ,0,head);
	  (*typeMap)[T] = -1;
	}
	if (T->isOpaqueTy()) typ = Val_int(9);
	if(T->isFunctionTy()) {
	  (*typeMap)[T] = depth;
	  typ = caml_alloc(2,1);
	  Store_field(typ,0,convert_aux(cast<FunctionType>(T)->getReturnType(), depth + 1, typeMap));
	  head = Val_int(0);
	  current = Val_int(0);
	  for (unsigned i = 0; i < cast<FunctionType>(T)->getNumParams(); ++i) {
	    cell = caml_alloc(2,0);
	    Store_field(cell,0,convert_aux(cast<FunctionType>(T)->getParamType(i),depth + 1, typeMap));
	    Store_field(cell,1,Val_int(0));
	    if (head == Val_int(0)) head = cell; 
	    if (current != Val_int(0)) Store_field(current,1,cell); 
	    current = cell;
	  }
	  Store_field(typ,1,head);
	  (*typeMap)[T] = -1;
	}
      }
    }
  
    
    CAMLreturn(typ);
  }

  value convert(const Type *T) {
    CAMLparam0();
    CAMLlocal1(v);

    tMap typeMap;
    v = convert_aux(T,0,&typeMap);
    typeMap.clear();

    CAMLreturn(v);
  }

  value convert(const Argument *A) {
    CAMLparam0();
    CAMLlocal2(s,arg);

    const Type *t = A->getType();
    s = caml_copy_string((A->getNameStr()).c_str());
    arg = caml_alloc(2,0);
    Store_field(arg,0,s);
    Store_field(arg,1,convert(t)); 

    CAMLreturn(arg);
  }

  value convert(const Constant *C) {
    CAMLparam0();
    CAMLlocal1(constant);

    bool ok = false;
    constant = Val_int(0);
    if (isa<ConstantPointerNull>(C) && (ok = true)) constant = Val_int(2);
    if (isa<ConstantInt>(C) && (ok = true)) {
      const ConstantInt *CI = cast<ConstantInt>(C);
      if (CI->getBitWidth() == 1) {
	if (CI->isZero()) constant = Val_int(1);
	else constant = Val_int(0);
      }
      else {
	constant = caml_alloc(1,0);
	Store_field(constant,0,caml_copy_int64(CI->getSExtValue()));
      }
    }
    if (isa<ConstantAggregateZero>(C) && (ok = true)) constant = Val_int(3);
    if (isa<UndefValue>(C) && (ok = true)) constant = Val_int(4);
    if (isa<ConstantFP>(C) && (ok = true)) {
      double d = cast<ConstantFP>(C)->getValueAPF().convertToDouble();
      constant = caml_alloc(1,1);
      Store_field(constant,0,caml_copy_double(d));
    }
    if (isa<Function>(C) && (ok = true)) {
      constant = caml_alloc(1,7);
      Store_field(constant,0,caml_copy_string(C->getNameStr().c_str()));
    }
    if (isa<GlobalVariable>(C) && (ok = true)) {
      constant = caml_alloc(1,6);
      Store_field(constant,0,caml_copy_string(C->getNameStr().c_str()));
    }
    if (isa<ConstantVector>(C) && (ok = true)) {
      constant = caml_alloc(1,4);
      Store_field(constant,0,convertIT<User::const_op_iterator>(C->op_begin(),C->op_end()));
    }
    if (isa<ConstantStruct>(C) && (ok = true)) {
      constant = caml_alloc(1,2);
      Store_field(constant,0,convertIT<User::const_op_iterator>(C->op_begin(),C->op_end()));
    }
    if (isa<ConstantArray>(C) && (ok = true)) {
      constant = caml_alloc(1,3);
      Store_field(constant,0,convertIT<User::const_op_iterator>(C->op_begin(),C->op_end()));
    }
    if (isa<ConstantExpr>(C)) {
      switch (cast<ConstantExpr>(C)->getOpcode()) {
      case Instruction::GetElementPtr: 
	ok = true;
	constant = caml_alloc(2,9);
	const Constant * CO = cast<Constant>(C->getOperand(0));
	Store_field(constant,0,convert(CO));
	// I worry about overloading resolution for the following one
	Store_field(constant,1,convertIT<User::const_op_iterator>(CO->op_begin(),CO->op_end()));
      }
    }
 
    if (!ok && stop_mode) { 
      errs() << "No rule to convert constant:\n" << *C << "\n";
      errs() << C->getNameStr();
      exit(1);
    }
    CAMLreturn(constant);
  }
  
  value convert(const Value *V) {
    CAMLparam0();
    CAMLlocal1(user);

    user = Val_int(0);
    // Get the variable correspong to the instruction
    if (isa<Instruction>(V)) {
      user = caml_alloc(1,1);
      Store_field(user,0,caml_copy_string(instNames.get(cast<const Instruction>(V)).c_str()));
    }      
    // convert the constant
    if (isa<Constant>(V)) {
      user = caml_alloc(1,0);
      Store_field(user,0,convert(cast<Constant>(V)));
    }
    // Treament of operands that are function arguments
    if (isa<Argument>(V)) {
      user = caml_alloc(1,1);
      std::string s = cast<Argument>(V)->getNameStr();
      Store_field(user,0,caml_copy_string(s.c_str()));
    }
    

    CAMLreturn(user);
    
  }
 
  value mkTop(const Value *V) {
    CAMLparam0();
    CAMLlocal1(tuple);

    tuple = caml_alloc(2,0);
    Store_field(tuple,0,convert(V->getType()));
    Store_field(tuple,1,convert(V));
    
    CAMLreturn(tuple);
  }

  // ConvertOption builds an OCaml option with the type and content of the value
  value convertOption(const Value *V) {
    CAMLparam0();
    CAMLlocal1(op);

    if (V == NULL) 
      op = Val_int(0);
    else {
      op = caml_alloc(1,0);
      Store_field(op,0,mkTop(V));
    }

    CAMLreturn(op);
  }
  
  int translateOpcode(int opcode) {
    switch (opcode) {
    case Instruction::Add: return 0;
    case Instruction::FAdd: return 0;
    case Instruction::Sub: return 1;
    case Instruction::FSub: return 1;
    case Instruction::Mul: return 2;
    case Instruction::FMul: return 2;
    case Instruction::UDiv: return 3;
    case Instruction::SDiv: return 4;
    case Instruction::FDiv: return 3;
    case Instruction::URem: return 4;
    case Instruction::SRem: return 5;
    case Instruction::FRem: return 6;
    case Instruction::Shl: return 5;
    case Instruction::LShr: return 6;
    case Instruction::AShr: return 7;
    case Instruction::And: return 7;
    case Instruction::Or: return 8;
    case Instruction::Xor: return 9;
    default: return 3000;
    }
  }

  value mkOpcode(const Instruction *I) {
    CAMLparam0();
    CAMLlocal1(op);

    bool b;
    int wrap = 0;
    switch (I->getOpcode()) {
    case Instruction::Add: 
    case Instruction::Sub:
    case Instruction::Mul:
    case Instruction::Shl:
      op = caml_alloc(1,translateOpcode(I->getOpcode()));
      if (cast<BinaryOperator>(I)->hasNoUnsignedWrap()) 
	if (cast<BinaryOperator>(I)->hasNoSignedWrap()) wrap = 3;
	else wrap = 2;
      else if (cast<BinaryOperator>(I)->hasNoSignedWrap()) wrap = 1;
      Store_field(op,0,Val_int(wrap)); 
      break;
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::LShr:
    case Instruction::AShr:
      op = caml_alloc(1,translateOpcode(I->getOpcode()));
      b = false;
      if (cast<BinaryOperator>(I)->isExact()) b = true;
      Store_field(op,0,b);
      break;
    default: 
      op = Val_int(translateOpcode(I->getOpcode()));
    }

    CAMLreturn(op);
  }




  // My Use of User may be awckward it's that a user can be an instruction
  // or a constant but rather that instruction and constant really inherit stuff
  // from User (as opposed to type or constant that are interfaces for instance)
  value mkBinInstruction(const Instruction *I) {
    CAMLparam0();
    CAMLlocal1(inst);

    inst = caml_alloc(5,5);
    Store_field(inst,0,caml_copy_string(instNames.get(I).c_str()));
    Store_field(inst,1,mkOpcode(I)); 
    Store_field(inst,2,convert(I->getType()));
    Store_field(inst,3,mkTop(I->getOperand(0))); 
    Store_field(inst,4,mkTop(I->getOperand(1)));

    CAMLreturn(inst);
  }

  value mkAlignment(unsigned al) {
    CAMLparam0();
    CAMLlocal1(alignment);

    if (al == 0) alignment = Val_int(0);
    else { 
      alignment = caml_alloc(1,0);
      Store_field(alignment,0,caml_copy_int32(al));
    }

    CAMLreturn(alignment);
  } 

  value convert(CallingConv::ID x) {
    return Val_int(x);
  }

  value convert(Attributes x) {
    CAMLparam0();
    CAMLlocal1(attr);

    switch (x) {
      // Param attributes
    case Attribute::ZExt: attr = Val_int(0);
    case Attribute::SExt: attr = Val_int(1);
    case Attribute::InReg: attr = Val_int(2);
    case Attribute::ByVal: attr = Val_int(3);
    case Attribute::StructRet: attr = Val_int(4);
    case Attribute::NoAlias: attr = Val_int(5);
    case Attribute::NoCapture: attr = Val_int(6);
    case Attribute::Nest: attr = Val_int(7);
      // Function attributes
    case Attribute::AlwaysInline: attr = Val_int(0);
    case Attribute::Hotpatch: attr = Val_int(1);
      // There should be an attribute nonlazybond but I can't find it
    case Attribute::InlineHint: attr = Val_int(3);
    case Attribute::Naked: attr = Val_int(4);
    case Attribute::NoImplicitFloat: attr = Val_int(5);
    case Attribute::NoInline: attr = Val_int(6);
    case Attribute::NoRedZone: attr = Val_int(7);
    case Attribute::NoReturn: attr = Val_int(8);
    case Attribute::NoUnwind: attr = Val_int(9);
    case Attribute::OptimizeForSize: attr = Val_int(10);
    case Attribute::ReadNone: attr = Val_int(11);
    case Attribute::ReadOnly: attr = Val_int(12);
    case Attribute::StackProtect: attr = Val_int(13);
    case Attribute::StackProtectReq: attr = Val_int(14);
    }

    CAMLreturn(attr);
  }

  value mkSome(value v) {
    CAMLparam1(v);
    CAMLlocal1(op);

    op = caml_alloc(1,0);
    Store_field(op,0,v);

    CAMLreturn(op);
  }

  value mkPredicate(int predicate) {
    CAMLparam0();
    CAMLlocal1(pred);
    
    if (predicate >= CmpInst::FIRST_FCMP_PREDICATE && predicate <= CmpInst::LAST_FCMP_PREDICATE)
      pred = Val_int(predicate);
    else pred = Val_int(predicate - 32);

    CAMLreturn(pred);
  }

  value mkTuple(value fst, value snd) {
    CAMLparam2(fst,snd);
    CAMLlocal1(tuple);

    tuple = caml_alloc(2,0);
    Store_field(tuple,0,fst);
    Store_field(tuple,1,snd);

    CAMLreturn(tuple);
  }

//   value mkList(std::list<value> l) {
//     value nl = Val_int(0);
//     value tmp = Val_int(0);
//     for (std::list<value>::iterator I = l.begin(); I != l.end(); ++I) {
//       value cell = caml_alloc(2,0);
//       Store_field(cell,0,*I);
//       Store_field(cell,1,Val_int(0));
//       if (nl == Val_int(0)) nl = cell;
//       if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
//       tmp = cell;
//     }
//     return nl;
//   }
 
  value convert(const Use *U) {
    CAMLparam0();
    CAMLlocal1(u);
    
    u = mkTop(U->get());

    CAMLreturn(u);
  }

  value convert(const unsigned int *i) {
    CAMLparam0();
    CAMLlocal1(v);

    v = caml_copy_int32(*i); // CAUTION, not sure what kind of int that should be

    CAMLreturn(v);
  }
  
  value convert(std::list<std::pair<BasicBlock *,Value *> >::const_iterator P) {
    CAMLparam0();
    CAMLlocal1(v);

    const BasicBlock *B = P->first;
    const Value *V = P->second;
    v = mkTuple(caml_copy_string(blockNames.get(B).c_str()),convert(V));

    CAMLreturn(v);
  }


  value convert(const Instruction *I) {
    CAMLparam0();
    CAMLlocal2(inst,lv); 

    //errs() << *I << "\n";
    instNames.assign(I);
    std::string var = instNames.get(I);
    inst = Val_int(0);
    if (I->isBinaryOp()) 
      inst = mkBinInstruction(I);
    if (isa<LoadInst>(I)) {
      inst = caml_alloc(4,7);
      const LoadInst *L = cast<LoadInst>(I);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,L->isVolatile()? Val_int(1):Val_int(0));
      Store_field(inst,2,mkTop(L->getPointerOperand()));
      Store_field(inst,3,mkAlignment(L->getAlignment()));
    }
    if (isa<AllocaInst>(I)) {
      inst = caml_alloc(4,6);
      const AllocaInst *A = cast<AllocaInst>(I);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(A->getAllocatedType()));
      Store_field(inst,2,convertOption(A->getArraySize())); 
      Store_field(inst,3,mkAlignment(A->getAlignment()));
    }
    if (isa<StoreInst>(I)) {
      inst = caml_alloc(4,8);
      const StoreInst *S = cast<StoreInst>(I);
      Store_field(inst,0,S->isVolatile()? Val_int(1):Val_int(0));
      Store_field(inst,1,mkTop(S->getValueOperand()));
      Store_field(inst,2,mkTop(S->getPointerOperand()));
      Store_field(inst,3,mkAlignment(S->getAlignment()));
    }
    if (isa<ReturnInst>(I)) {
      const ReturnInst *R = cast<ReturnInst>(I);
      inst = caml_alloc(1,0);
      Store_field(inst,0,convertOption(R->getReturnValue()));
    }
    if (isa<BranchInst>(I)) {
      inst = caml_alloc(3,1);
      const BranchInst *B = cast<BranchInst>(I);
      if (B->isConditional()) {
	Store_field(inst,0,mkSome(mkTop(B->getCondition())));
	std::string label1 = blockNames.get(B->getSuccessor(0));
	Store_field(inst,1,caml_copy_string(label1.c_str()));
	std::string label2 = blockNames.get(B->getSuccessor(1));
	Store_field(inst,2,mkSome(caml_copy_string(label2.c_str())));
      }
      else {
	Store_field(inst,0,Val_int(0));
	std::string label = blockNames.get(B->getSuccessor(0));
	Store_field(inst,1,caml_copy_string(label.c_str()));
	Store_field(inst,2,Val_int(0));
      }
    }
    if (isa<CmpInst>(I)) {
      int code;
      const CmpInst *C = cast<CmpInst>(I);
      if (isa<ICmpInst>(I)) code = 11;
      else code = 12;
      inst = caml_alloc(5,code);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkPredicate(C->getPredicate()));
      Store_field(inst,2,convert(C->getType()));
      Store_field(inst,3,mkTop(C->getOperand(0)));
      Store_field(inst,4,mkTop(C->getOperand(1)));
    }
    if (isa<UnreachableInst>(I)) inst = Val_int(1);
    if (isa<UnwindInst>(I)) inst = Val_int(0);
    if (isa<ExtractElementInst>(I)) {
      const ExtractElementInst *E = cast<ExtractElementInst>(I);
      inst = caml_alloc(3,15);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getVectorOperand()));
      Store_field(inst,2,convert(E->getIndexOperand()));
    }
    if (isa<InsertElementInst>(I)) {
      const InsertElementInst *E = cast<InsertElementInst>(I);
      inst = caml_alloc(4,16);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getOperand(0)));
      Store_field(inst,2,mkTop(E->getOperand(1)));
      Store_field(inst,3,convert(E->getOperand(2)));
    }
    if (isa<ShuffleVectorInst>(I)) {
      const ShuffleVectorInst *E = cast<ShuffleVectorInst>(I);
      inst = caml_alloc(4,17);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getOperand(0)));
      Store_field(inst,2,mkTop(E->getOperand(1)));
      Store_field(inst,3,mkTop(E->getOperand(2)));
    }
     if (isa<PHINode>(I)) {
       const PHINode *N = cast<PHINode>(I);
       inst = caml_alloc(3,13);
       Store_field(inst,0,caml_copy_string(var.c_str()));
       Store_field(inst,1,convert(N->getType()));
       std::list<std::pair<BasicBlock*,Value*> > l;
       for (unsigned i = 0 ; i < N->getNumIncomingValues(); ++i) 
	 l.push_back(std::pair<BasicBlock*,Value*>(N->getIncomingBlock(i),N->getIncomingValue(i)));
       
       lv = convertIT<std::list<std::pair<BasicBlock*,Value*> >::const_iterator>(l.begin(),l.end());
       Store_field(inst,2,lv);
       l.clear();
     }
    if (isa<CastInst>(I)) {
      const CastInst *C = cast<CastInst>(I);
      inst = caml_alloc(4,10);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,Val_int(C->getOpcode() - 30));
      Store_field(inst,2,mkTop(C->getOperand(0)));
      Store_field(inst,3,convert(C->getDestTy()));
    }
    if (isa<ExtractValueInst>(I)) {
      const ExtractValueInst *E = cast<ExtractValueInst>(I);
      inst = caml_alloc(3,18);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(E->getAggregateOperand()));
      lv = convertIT<ExtractValueInst::idx_iterator>(E->idx_begin(),E->idx_end()); 
      Store_field(inst,2,lv);
    }
    if (isa<InsertValueInst>(I)) {
      const InsertValueInst *E = cast<InsertValueInst>(I);
      inst = caml_alloc(4,19);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(E->getAggregateOperand()));
      Store_field(inst,2,convert(E->getInsertedValueOperand()));
      lv = convertIT<InsertValueInst::idx_iterator>(E->idx_begin(),E->idx_end());
      Store_field(inst,3,lv);
    }
    if (isa<SwitchInst>(I)) inst = caml_alloc(4,2);
//       { 
//       const SwitchInst *S = cast<SwitchInst>(I);
//       inst = caml_alloc(4,2);
//       Store_field(inst,0,caml_copy_string(var.c_str()));
//       Store_field(inst,1,mkTop(S->getCondition()));
//       Store_field(inst,2,caml_copy_string(blockNames.get(S->getDefaultDest())));
//       Store_field(inst,3,);
//     }
    if (isa<IndirectBrInst>(I)) inst = caml_alloc(2,3);
    if (isa<InvokeInst>(I)) inst = caml_alloc(8,4);
    if (isa<GetElementPtrInst>(I)) { 
      const GetElementPtrInst *G = cast<GetElementPtrInst>(I);
      inst = caml_alloc(4,9);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,G->isInBounds()?Val_int(1):Val_int(0));
      Store_field(inst,2,mkTop(G->getPointerOperand()));
      lv = convertIT<User::const_op_iterator>(G->idx_begin(),G->idx_end());      
      Store_field(inst,3,lv);
    }
    if (isa<SelectInst>(I)) {
      const SelectInst *S = cast<SelectInst>(I);
      inst = caml_alloc(4,14);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(S->getCondition()));
      Store_field(inst,2,mkTop(S->getTrueValue()));
      Store_field(inst,3,mkTop(S->getFalseValue()));
    }
    if (isa<VAArgInst>(I)) inst = caml_alloc(1,21);
    if (isa<CallInst>(I))  {
      const CallInst *C = cast<CallInst>(I);
      inst = caml_alloc(8,20);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,C->isTailCall()?Val_int(1):Val_int(0));
      Store_field(inst,2,convert(C->getCallingConv()));
      Store_field(inst,3,Val_int(0)); // NIY
      Store_field(inst,4,convert(C->getType()));
      Store_field(inst,5,mkTop(C->getCalledValue()));
      // The -1 that follows is important. The last value in the user array is the function itself.
      lv = convertIT<User::const_op_iterator>(C->op_begin(),C->op_end()-1);
      Store_field(inst,6,lv); 
      Store_field(inst,7,Val_int(0)); // NIY
    }
    if (isa<IndirectBrInst>(I)) {
      inst = caml_alloc(2,3);
      const IndirectBrInst *B = cast<IndirectBrInst>(I);
      Store_field(inst,0,mkTop(B->getAddress()));
      Store_field(inst,1,convertIT<>(B->getDestination(0),B->getDestination(0)+B->getNumDestinations()));
    } // This last store field is wacky!!!

//     if (isa<InvokeInst>(I)) {
//       inst = caml_alloc(8,4);
//       const InvokeInst *I = cast<InvokeInst>(I);
//       Store_field(inst,0,);
//       Store_field(inst,1,);
//       Store_field(inst,2,);
//       Store_field(inst,3,);
//       Store_field(inst,4,);
//       Store_field(inst,5,);
//       Store_field(inst,6,);
//       Store_field(inst,7,);
//     }

    var.clear();
    
    CAMLreturn(inst);
  }
    
  value convert(const BasicBlock *B) {
    CAMLparam0();
    CAMLlocal2(block,l);

    block = caml_alloc(2,0);
    Store_field(block,0,caml_copy_string(blockNames.get(B).c_str()));
    l = convertIT<BasicBlock::const_iterator>(B->begin(),B->end());
    Store_field(block,1,l);

    CAMLreturn(block);
  }

  value convert (const Function *F) {
    CAMLparam0();
    CAMLlocal4(f,s,args,body);

    errs() << "Converting " << F->getNameStr() << "...";
    instNames.clear();
    blockNames.clear();
    f = caml_alloc(12,0);
    s = caml_copy_string(F->getNameStr().c_str());
    Store_field(f,5,s);
    args = convertIT<Function::const_arg_iterator>(F->arg_begin(),F->arg_end());
    Store_field(f,6,args);
    for (Function::const_iterator I = F->getBasicBlockList().begin(), 
	   E = F->getBasicBlockList().end(); 
	 I != E; ++I) 
      blockNames.assign(I); 
    body = convertIT<Function::const_iterator>(F->begin(),F->end());
    Store_field(f,11,body);
    errs() << "OK\n";
    

    CAMLreturn(f);    
  }
  
  value convert(TypeSymbolTable::const_iterator TI) {
    CAMLparam0();
    CAMLlocal1(entry);

    entry = caml_alloc(2,0);
    Store_field(entry,0,caml_copy_string(TI->first.c_str()));
    // Communicating the name of the named type being computed to convert through 
    // the global variable
    converted = TI->first;
    Store_field(entry,1,convert(TI->second));

    CAMLreturn(entry);
  }

  value convert(const GlobalVariable *GV) {
    CAMLparam0();
    CAMLlocal2(global,ini);

    global = caml_alloc(9,0);
    Store_field(global,0,caml_copy_string(GV->getNameStr().c_str()));
    Store_field(global,6,GV->isThreadLocal()? Val_int(1): Val_int(0));
    Store_field(global,8,GV->isConstant()? Val_int(1):Val_int(0));
    if (GV->hasInitializer()) {
      ini = caml_alloc(1,0);
      Store_field(ini,0,convert(GV->getInitializer()));
    }
    else ini = Val_int(0);
    Store_field(global,5,ini);

    CAMLreturn(global);
  }

  value convert(const Module *M) {
    CAMLparam0();
    CAMLlocal2(module,v);
    
    module = caml_alloc(7,0);
    Store_field(module,0,caml_copy_string(M->getModuleIdentifier().c_str()));
    v = convertIT<Module::const_iterator>(M->begin(),M->end()); 
    Store_field(module,2,convertIT<Module::const_global_iterator>(M->global_begin(),M->global_end()));
    Store_field(module,3,v);
    Store_field(module,6,convertIT<TypeSymbolTable::const_iterator>(M->getTypeSymbolTable().begin(),M->getTypeSymbolTable().end()));   
    CAMLreturn(module);
  }

  bool Ali::runOnModule(Module &M) {
    CAMLparam0();
    CAMLlocal2(v,w);

    Maccess = &M;
    v = convert(&M);
    errs() << "Transformation\n" ;
    w = caml_callback_exn(*caml_named_value("transform"),v); 
    if (Is_exception_result(w)) 
      errs() << "An exception occured in the OCaml code.\n" ;

    CAMLreturnT(bool,false);
  }

  char Ali::ID = 0;

  static RegisterPass<Ali> X("ali", "Ali Pass", false , false );

}
