#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Constants.h"

#include <map>

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

  std::map<Instruction*,int> mmm; 
  int counter = 0;

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

  value convert(const Constant *C) {
    
    value constant = Val_int(0);

    if (isa<ConstantInt>(C)) {
      constant = Val_int(0);
    }
    else if (isa<ConstantFP>(C)) {
      constant = Val_int(1);
    }
    else {
      constant = Val_int(2);
    }
    return constant;
  }

  value convert(const Value *V) {
    value user = Val_int(0);
    if (isa<Constant>(V)) {
      user = convert(cast<Constant>(V));
      }
   
    return user;
  }
  
  value mkBinInstruction(int i) {
    value inst = caml_alloc(3,7);
    Store_field(inst,0,Val_int(i - 8)); // WATCH OUT
    Store_field(inst,1,Val_int(0));
    Store_field(inst,2,Val_int(0));
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
      inst = mkBinInstruction(op);
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
    Store_field(arg,1,Val_int(0)); // TODO
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
    value tmp = caml_callback(*caml_named_value("transform"),v); 
    errs() << "\n\nEnd\n\n";
    errs() << "Caml: " << F.getNameStr() << "\n";
    return false;
  }

  char Ali::ID = 0;

  static RegisterPass<Ali> X("ali", "Ali Pass", false , false );

}
