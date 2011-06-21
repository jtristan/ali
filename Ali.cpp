#include "llvm/Pass.h"
#include "llvm/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Constants.h"

extern "C"{
#include </usr/local/lib/ocaml/caml/mlvalues.h>
#include </usr/local/lib/ocaml/caml/alloc.h>
#include </usr/local/lib/ocaml/caml/callback.h>
#include </usr/local/lib/ocaml/caml/memory.h>
}

using namespace llvm;

namespace {

 struct Ali : public FunctionPass {
   static char ID;
   Ali() : FunctionPass(ID) {
     char* t[1] = {"crap"}; 
     caml_startup(t);         
   }
   
   virtual bool runOnFunction(Function &F);

 };

  value convertConstant(const Constant *C) {
    
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

  value convertUser(const Value *V) {
    value user = Val_int(0);
    if (isa<Constant>(V)) {
      user = convertConstant(cast<Constant>(V));
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
    value inst = caml_alloc(1,i); // WATCH OUT
    Store_field(inst,0,Val_int(0)); // DUMMY, WILL NEED REFINEMENT
    return inst;
  }

  value mkMemInstruction(int i) {
    value inst = caml_alloc(1,i-18); //WATCH OUT
    Store_field(inst,0,Val_int(0));
    return inst;
  }

  value convertInst(const Instruction *I) {
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

//   template <class T> value convert(const iplist<T> *L) {
//     value l = Val_int(0);
//     value tmp = Val_int(0);
//     for (iplist<T>::const_iterator I = L->begin(), E = L->end(); I != E; ++I) {
//       value cell = caml_alloc(2,0);
//       value a = convert(I); 
//       Store_field(cell,0,a);
//       Store_field(cell,1,Val_int(0));
//       if (l == Val_int(0)) l = cell;
//       if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
//       tmp = cell;
//     }
    
//     return l;
//   }  
  
  
  value convertInstList(const BasicBlock::InstListType *L) {
    errs() << "Converting Inst List\n";
    value l = Val_int(0);
    value tmp = Val_int(0);
    for (BasicBlock::const_iterator I = L->begin(), E = L->end(); I != E; ++I) {
      value cell = caml_alloc(2,0);
      value a = convertInst(I);
      Store_field(cell,0,a);
      Store_field(cell,1,Val_int(0));
      if (l == Val_int(0)) l = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    
    return l;
  }    

  value convertBasicBlock(const BasicBlock *B) {
    errs() << "Converting basic block\n";
    value block = caml_alloc(2,0);
    Store_field(block,0,caml_copy_string(B->getNameStr().c_str()));
    Store_field(block,1,convertInstList(&B->getInstList()));
    return block;
  }


  value convertBody(const Function::BasicBlockListType *L) {
    errs() << "Converting Body\n";
    value l = Val_int(0);
    value tmp = Val_int(0);
    for (Function::const_iterator I = L->begin(), E = L->end(); I != E; ++I) {
      value cell = caml_alloc(2,0);
      value a = convertBasicBlock(I);
      Store_field(cell,0,a);
      Store_field(cell,1,Val_int(0));
      if (l == Val_int(0)) l = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    
    return l;
  }

  value convertArgument(const Argument *A) {
    errs() << "converting argument\n";
    const Type *t = A->getType();
    value s = caml_copy_string((A->getNameStr()).c_str());
    value arg = caml_alloc(2,0);
    // First name, Second type
    Store_field(arg,0,s);
    Store_field(arg,1,Val_int(0)); // TODO
    return arg;
  }

  value convertArgumentList(const Function::ArgumentListType *L) {
    errs() << "Converting arguments list\n";
    value l = Val_int(0);
    value tmp = l;
    for (Function::const_arg_iterator I = L->begin(), E = L->end(); I != E; ++I) {
      value cell = caml_alloc(2,0);
      value a = convertArgument(I);
      Store_field(cell,0,a);
      Store_field(cell,1,Val_int(0));
      if (l == Val_int(0)) l = cell;
      if (tmp != Val_int(0)) Store_field(tmp,1,cell); 
      tmp = cell;
    }
    
    return l;
  }

  value convertFunction (const Function *F) {
    errs () << "Converting function\n";
    value f = caml_alloc(3,0);
    value s = caml_copy_string(F->getNameStr().c_str());
    Store_field(f,0,s);
    value args = convertArgumentList(&F->getArgumentList());
    Store_field(f,1,args);
    value body = convertBody(&F->getBasicBlockList());
    Store_field(f,2,body);
    
    return f;
    
  }

  bool Ali::runOnFunction(Function &F) {
    errs() << "Conversion\n\n";
    value v = convertFunction(&F);
    errs() << "\n\nTransformation\n\n";
    value tmp = caml_callback(*caml_named_value("transform"),v); 
    errs() << "\n\nEnd\n\n";
    errs() << "Caml: " << F.getNameStr() << "\n";
    return false;
  }

  char Ali::ID = 0;

  static RegisterPass<Ali> X("caml", "Caml Pass", false , false );

}
