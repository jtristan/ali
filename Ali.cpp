#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Constants.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"

#include <map>
#include <sstream>

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

  template <class T, class IT> value convertList(const iplist<T> *L) {
    errs() << "Converting list\n";
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

  value convert(const Type *T) {
    errs() << "Converting types\n";
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


  value convert(const Constant *C) {
    errs() << "Converting Constant\n";
    value constant = Val_int(0);

    if (isa<ConstantInt>(C)) {
      constant = caml_alloc(1,0);
      value v = caml_copy_int64(cast<ConstantInt>(C)->getSExtValue());
      Store_field(constant,0,v);
    }
    else if (isa<ConstantFP>(C)) {
      constant = Val_int(1);
    }
    else {
      constant = Val_int(5);
    }
    return constant;
  }
  
  // Is it really User? What is the Op inheriting from User?
  value convert(const User *U) {
    errs() << "Converting User\n";
    value user = Val_int(0);
    // Get the variable correspong to the instruction
    if (isa<Instruction>(U)) {
      user = caml_alloc(1,1);
      std::string var;
      std::ostringstream out;
      out << U;
      var = out.str();
      Store_field(user,0,caml_copy_string(var.c_str()));
    }      
    // convert the constant
    if (isa<Constant>(U)) {
      user = caml_alloc(1,0);
      Store_field(user,0,convert(cast<Constant>(U)));
    }
    // Shoud there be a 3rd case for Operator???

    return user;
    
  }

  // My Use of User may be awckward it's that a user can be an instruction
  // or a constant but rather that instruction and constant really inherit stuff
  // from User (as opposed to type or constant that are interfaces for instance)
  value mkBinInstruction(const Instruction *I) {
    value inst = caml_alloc(3,7);
    Store_field(inst,0,Val_int(I->getOpcode() - 8)); // WATCH OUT
    Store_field(inst,1,convert(cast<User>(I->getOperand(0)))); 
    Store_field(inst,2,convert(cast<User>(I->getOperand(1))));
    return inst;
  }

  value mkTermInstruction(int i) {
    value inst = caml_alloc(1,i-1); // WATCH OUT
    Store_field(inst,0,Val_int(0)); // DUMMY, WILL NEED REFINEMENT
    return inst;
  }

  value mkMemInstruction(int i) {
    value inst = caml_alloc(1,i-18); //WATCH OUT
    Store_field(inst,0,Val_int(0));
    return inst;
  }

  value convert(const Instruction *I) {
    errs() << "Instruction: " << *I << "\n";
    value inst = Val_int(0);
    int op = I->getOpcode();
    if (I->isTerminator())
      inst = mkTermInstruction(op);
    if (I->isBinaryOp()) 
      inst = mkBinInstruction(I);
    if (op >= 26 && op <= 29)
      inst = mkMemInstruction(op);

    return inst;
  }
    
  value convert(const BasicBlock *B) {
    errs() << "Converting basic block\n";
    value block = caml_alloc(2,0);
    Store_field(block,0,caml_copy_string(B->getNameStr().c_str()));
    value l = convertList<Instruction,BasicBlock::const_iterator>(&B->getInstList());
    Store_field(block,1,l);
    return block;
  }

  value convert(const Argument *A) {
    errs() << "converting argument\n";
    const Type *t = A->getType();
    value s = caml_copy_string((A->getNameStr()).c_str());
    value arg = caml_alloc(2,0);
    // First name, Second type
    Store_field(arg,0,s);
    Store_field(arg,1,convert(t)); 
    return arg;
  }

  value convert (const Function *F) {
    errs () << "Converting function\n";
    value f = caml_alloc(3,0);
    value s = caml_copy_string(F->getNameStr().c_str());
    Store_field(f,0,s);
    value args = convertList<Argument,Function::const_arg_iterator>(&F->getArgumentList());
    Store_field(f,1,args);
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
