#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Constants.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"
#include "llvm/InstrTypes.h"
#include "llvm/Instructions.h"
#include "llvm/IntrinsicInst.h"

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

  class Ali : public FunctionPass {
  public:
    static char ID;
    Ali() : FunctionPass(ID) {
      char* t[1] = {"crap"}; 
      caml_startup(t);         
    }
    
    virtual bool runOnFunction(Function &F);
    
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
  };

  Namer<const Instruction*> instNames;
  Namer<const BasicBlock*> blockNames;

  template <class T, class IT> value convertList(const iplist<T> *L) {
    value l = Val_int(0);
    value tmp = Val_int(0);
    for (IT I = L->begin(), E = L->end(); I != E; ++I) {
      value cell = caml_alloc(2,0);
      value a = convert(I); 
      Store_field(cell,0,a);
      Store_field(cell,1,Val_int(0));
      if (l == Val_int(0)) l = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    
    return l;
  }  


  // conversion of types
  value convert(const Type *T) {
    value typ = Val_int(0);
    if (T->isPrimitiveType()) typ = Val_int(T->getTypeID());
    if (T->isIntegerTy()) {
      typ = caml_alloc(1,0);
      value bitwidth = caml_copy_int32(cast<IntegerType>(T)->getBitWidth());
      Store_field(typ,0,bitwidth);
    }
    if (isa<SequentialType>(T)) {
      typ = caml_alloc(2,T->getTypeID() - 9); 
      value arg = convert(cast<SequentialType>(T)->getElementType());
      Store_field(typ,0,arg);
    }
    if (T->isStructTy()) {
      typ = caml_alloc(1,2);
      value current = Val_int(0);
      for (unsigned i = 0; i < cast<StructType>(T)->getNumElements(); ++i) {
	value cell = caml_alloc(2,0);
	Store_field(cell,0,convert(cast<StructType>(T)->getElementType(i)));
	Store_field(cell,1,Val_int(0));
	Store_field(current,1,cell);
	if (typ == Val_int(0)) Store_field(typ,0,cell);
	current = cell;
      }
    }
    if (T->isOpaqueTy()) typ = Val_int(9);
    if(T->isFunctionTy()) {
      typ = caml_alloc(2,1);
      Store_field(typ,0,convert(cast<FunctionType>(T)->getReturnType()));
      value current = Val_int(0);
      for (unsigned i = 0; i < cast<FunctionType>(T)->getNumParams(); ++i) {
	value cell = caml_alloc(2,0);
	Store_field(cell,0,convert(cast<FunctionType>(T)->getParamType(i)));
	Store_field(cell,1,Val_int(0));
	Store_field(current,1,cell);
	if (typ == Val_int(0)) Store_field(typ,1,cell);
	current = cell;
      }
    }

    return typ;
  }

  value convert(const Argument *A) {
    const Type *t = A->getType();
    value s = caml_copy_string((A->getNameStr()).c_str());
    value arg = caml_alloc(2,0);
    // First name, Second type
    Store_field(arg,0,s);
    Store_field(arg,1,convert(t)); 
    return arg;
  }

  value convert(const Constant *C) {
    value constant;

    if (isa<ConstantPointerNull>(C)) constant = Val_int(2);
    if (isa<ConstantInt>(C)) {
      const ConstantInt *CI = cast<ConstantInt>(C);
      if (CI->getBitWidth() == 1) {
	if (CI->isZero()) constant = Val_int(1);
	else constant = Val_int(0);
      }
      else {
	constant = caml_alloc(1,0);
	value v = caml_copy_int64(CI->getSExtValue());
	Store_field(constant,0,v);
      }
    }
    if (isa<ConstantAggregateZero>(C)) constant = Val_int(3);
    if (isa<UndefValue>(C)) constant = Val_int(4);
    if (isa<ConstantFP>(C)) {
      errs() << "Floating Point Constants NYI\n";
    }
    
    return constant;
  }
  
  value convert(const Value *V) {
    value user = Val_int(0);
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
    

    return user;
    
  }
 
  value mkTop(const Value *V) {
    value tuple = caml_alloc(2,0);
    Store_field(tuple,0,convert(V->getType()));
    Store_field(tuple,1,convert(V));
    return tuple;
  }

  // ConvertOption builds an OCaml option with the type and content of the value
  value convertOption(const Value *V) {
    value op;
    if (V == NULL) 
      op = Val_int(0);
    else {
      op = caml_alloc(1,0);
      Store_field(op,0,mkTop(V));
    }
    return op;
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
    value op;
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
      return op;
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::LShr:
    case Instruction::AShr:
      op = caml_alloc(1,translateOpcode(I->getOpcode()));
      b = false;
      if (cast<BinaryOperator>(I)->isExact()) b = true;
      Store_field(op,0,b);
      return op;
    default: 
      return (Val_int(translateOpcode(I->getOpcode())));
    }
  }



  // My Use of User may be awckward it's that a user can be an instruction
  // or a constant but rather that instruction and constant really inherit stuff
  // from User (as opposed to type or constant that are interfaces for instance)
  value mkBinInstruction(const Instruction *I) {
    value inst = caml_alloc(5,5);
    Store_field(inst,0,caml_copy_string(instNames.get(I).c_str()));
    Store_field(inst,1,mkOpcode(I)); 
    Store_field(inst,2,convert(I->getType()));
    Store_field(inst,3,mkTop(I->getOperand(0))); 
    Store_field(inst,4,mkTop(I->getOperand(1)));
    return inst;
  }

  value mkAlignment(unsigned al) {
    value alignment;
    if (al == 0) alignment = Val_int(0);
    else { 
      alignment = caml_alloc(1,0);
      Store_field(alignment,0,caml_copy_int32(al));
    }
    return alignment;
  } 

  value convert(CallingConv::ID x) {
    return Val_int(x);
  }

  value convert(Attributes x) {
    value attr;
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
    return attr;
  }

  value mkSome(value v) {
    value op = caml_alloc(1,0);
    Store_field(op,0,v);
    return op;
  }

  value mkPredicate(int predicate) {
    if (predicate >= CmpInst::FIRST_FCMP_PREDICATE && predicate <= CmpInst::LAST_FCMP_PREDICATE)
      return Val_int(predicate);
    else return Val_int(predicate - 32);
  }

  value mkTuple(value fst, value snd) {
    value tuple = caml_alloc(2,0);
    Store_field(tuple,0,fst);
    Store_field(tuple,1,snd);
    return tuple;
  }

  value mkList(std::list<value> l) {
    value nl = Val_int(0);
    value tmp = Val_int(0);
    for (std::list<value>::iterator I = l.begin(); I != l.end(); ++I) {
      value cell = caml_alloc(2,0);
      Store_field(cell,0,*I);
      Store_field(cell,1,Val_int(0));
      if (nl == Val_int(0)) nl = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    return nl;
  }
 
  value convert(const Instruction *I) {
    errs() << *I << "\n";
    instNames.assign(I);
    std::string var = instNames.get(I);
    value inst = Val_int(0);
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
      std::list<value> l;
      for (unsigned i = 0 ; i < N->getNumIncomingValues(); ++i) {
	value lab = caml_copy_string(blockNames.get(N->getIncomingBlock(i)).c_str());
	value v = convert(N->getIncomingValue(i)); 
	value t = mkTuple(lab,v);
	l.push_back(t);
      }
      Store_field(inst,2,mkList(l));
    }
    if (isa<CastInst>(I)) {
      const CastInst *C = cast<CastInst>(I);
      inst = caml_alloc(4,10);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,Val_int(C->getOpcode() - 30));
      Store_field(inst,2,mkTop(C->getOperand(0)));
      Store_field(inst,3,convert(C->getDestTy()));
    }
    if (isa<ExtractValueInst>(I)) inst = caml_alloc(4,18);
    if (isa<InsertValueInst>(I)) inst = caml_alloc(5,19);
    if (isa<SwitchInst>(I)) inst = caml_alloc(4,2);
    if (isa<IndirectBrInst>(I)) inst = caml_alloc(2,3);
    if (isa<InvokeInst>(I)) inst = caml_alloc(8,4);
    if (isa<GetElementPtrInst>(I)) inst = caml_alloc(3,9);
    if (isa<SelectInst>(I)) inst = caml_alloc(5,14);
    if (isa<VAArgInst>(I)) inst = caml_alloc(1,21);
    if (isa<IntrinsicInst>(I)) inst = caml_alloc(1,22);
    if (isa<CallInst>(I)) inst = caml_alloc(1,20); 

//     if (isa<ExtractValueInst>(I)) {
//       const ExtractValueInst *E = cast<ExtractValueInst>(I);
//       inst = caml_alloc(4,18);
//       Store_field(inst,0,caml_copy_string(var.c_str()));
//       Store_field(inst,1,convert(E->getAggregateOperand()));
//       Store_field(inst,2,);
//       Store_field(inst,3,);
//     }
//     if (isa<InsertValueInst>(I)) {
//       const InsertValueInst *E = cast<InsertValueInst>(I);
//       inst = caml_alloc(5,19);
//       Store_field(inst,0,caml_copy_string(var.c_str()));
//       Store_field(inst,1,convert(E->getAggregateOperand()));
//       Store_field(inst,2,convert(E->getInsertedValueOperand()));
//       Store_field(inst,3,);
//       Store_field(inst,4,);
//     }
//     if (isa<SwitchInst>(I)) {
//       inst = caml_alloc(4,2);
//       const SwitchInst *I = cast<SwitchInst>(I);
//       Store_field(inst,0,convert(I->getCondition()->getType()));
//       Store_field(inst,1,convert(I->getCondition()));
//       Store_field(inst,2,);
//       Store_field(inst,3,);
//     } 
//     if (isa<IndirectBrInst>(I)) {
//       inst = caml_alloc(3,3);
//       const IndirectBrInst *I = cast<IndirectBrInst>(I);
//       Store_field(inst,0,);
//       Store_field(inst,1,);
//       Store_field(inst,2,);
//     } 
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

     

    return inst;
  }
    
  value convert(const BasicBlock *B) {
    value block = caml_alloc(2,0);
    Store_field(block,0,caml_copy_string(blockNames.get(B).c_str()));
    value l = convertList<Instruction,BasicBlock::const_iterator>(&B->getInstList());
    Store_field(block,1,l);
    return block;
  }

  value convert (const Function *F) {
    value f = caml_alloc(3,0);
    value s = caml_copy_string(F->getNameStr().c_str());
    Store_field(f,0,s);
    value args = convertList<Argument,Function::const_arg_iterator>(&F->getArgumentList());
    Store_field(f,1,args);
    for (Function::const_iterator I = F->getBasicBlockList().begin(), 
	   E = F->getBasicBlockList().end(); 
	 I != E; ++I) 
      blockNames.assign(I); 
    value body = convertList<BasicBlock,Function::const_iterator>(&F->getBasicBlockList());
    Store_field(f,2,body);
    
    return f;
    
  }

  bool Ali::runOnFunction(Function &F) {
    errs() << "Conversion\n\n";
    value v = convert(&F);
    errs() << "\n\nTransformation\n\n";
    caml_callback(*caml_named_value("transform"),v); 
    errs() << "\n\nEnd\n\n";
    errs() << "Caml: " << F.getNameStr() << "\n";
    return false;
  }

  char Ali::ID = 0;

  static RegisterPass<Ali> X("ali", "Ali Pass", false , false );

}
