//===- Ali.cpp - Conversion of the LLVM IR to an OCAML value --------------===//
//
//                                  ALI
//
// Copyrigth (c) 2011 Jean-Baptiste Tristan
// This file is distributed under the MIT license.
// See LICENSE for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the conversion of the intermediate representation of
// an LLVM program into an OCaml value.
//
//===----------------------------------------------------------------------===//

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

#include "llvm/LLVMContext.h"

#include <map>
#include <sstream>
#include <exception>
#include <list>
#include <vector>


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
      out << ++counter;
      var = out.str();
      m[t] = var; }
    void assign(T t,StringRef Name) {
      std::ostringstream buf;
      char* s = strdup(Name.str().c_str());
      m[t] = std::string(s); //buf.str(); 
      //m[t] = Name.str(); 
    }
    std::string get(T t) { return m[t]; }
    void clear() { counter = 0; m.clear(); }
    void reg(T t, std::string name) { m[t] = name; } 
  };

  Namer<const Instruction*> instNames;
  Namer<const BasicBlock*> blockNames;

  /*
    Helper functions to build derived OCaml values lists, options, and tuples.
  */

  value mkTuple(value fst, value snd) {
    CAMLparam2(fst,snd);
    CAMLlocal1(tuple);

    tuple = caml_alloc(2,0);
    Store_field(tuple,0,fst);
    Store_field(tuple,1,snd);

    CAMLreturn(tuple);
  }

  value mkSome(value v) {
    CAMLparam1(v);
    CAMLlocal1(op);

    op = caml_alloc(1,0);
    Store_field(op,0,v);

    CAMLreturn(op);
  }

  /*
    check is a debug function that stops the conversion if something could
    not be translated. 
   */

  void check(bool ok, std::string s) {

    bool stop_mode = false;
    if (!ok && stop_mode) { 
      errs() << "No rule to convert " << s << "\n"; 
      exit(1);
    }
  }
  
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

  /*
    The id_ functions give the OCaml encoding for various LLVM enums
   */

  int id_opcode(int opcode) {
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
    default: exit(1);
    }
  }

  int id_cc(CallingConv::ID x) {
    switch (x) {
    case CallingConv::C: return 0;
    case CallingConv::Fast: return 1;
    case CallingConv::Cold: return 2;
    default: exit(1);
    }
  }

//   value convert(Attributes x) {
//     CAMLparam0();
//     CAMLlocal1(attr);

//     switch (x) {
//       // Param attributes
//     case Attribute::ZExt: attr = Val_int(0);
//     case Attribute::SExt: attr = Val_int(1);
//     case Attribute::InReg: attr = Val_int(2);
//     case Attribute::ByVal: attr = Val_int(3);
//     case Attribute::StructRet: attr = Val_int(4);
//     case Attribute::NoAlias: attr = Val_int(5);
//     case Attribute::NoCapture: attr = Val_int(6);
//     case Attribute::Nest: attr = Val_int(7);
//       // Function attributes
//     case Attribute::AlwaysInline: attr = Val_int(0);
//     case Attribute::Hotpatch: attr = Val_int(1);
//       // There should be an attribute nonlazybond but I can't find it
//     case Attribute::InlineHint: attr = Val_int(3);
//     case Attribute::Naked: attr = Val_int(4);
//     case Attribute::NoImplicitFloat: attr = Val_int(5);
//     case Attribute::NoInline: attr = Val_int(6);
//     case Attribute::NoRedZone: attr = Val_int(7);
//     case Attribute::NoReturn: attr = Val_int(8);
//     case Attribute::NoUnwind: attr = Val_int(9);
//     case Attribute::OptimizeForSize: attr = Val_int(10);
//     case Attribute::ReadNone: attr = Val_int(11);
//     case Attribute::ReadOnly: attr = Val_int(12);
//     case Attribute::StackProtect: attr = Val_int(13);
//     case Attribute::StackProtectReq: attr = Val_int(14);
//     }

//     CAMLreturn(attr);
//   }

  int id_linkage(GlobalValue::LinkageTypes l) {
    int id = 0;

    switch (l) {
    case GlobalValue::PrivateLinkage: return 0;
    case GlobalValue::LinkerPrivateLinkage: return 1;
    case GlobalValue::LinkerPrivateWeakLinkage: return 2;
    case GlobalValue::LinkerPrivateWeakDefAutoLinkage: return 3;
    case GlobalValue::InternalLinkage: return 4;
    case GlobalValue::AvailableExternallyLinkage: return 5;
    case GlobalValue::LinkOnceAnyLinkage: return 6;
    case GlobalValue::WeakAnyLinkage: return 7;
    case GlobalValue::CommonLinkage: return 8;
    case GlobalValue::AppendingLinkage: return 9;
    case GlobalValue::ExternalWeakLinkage: return 10;
    case GlobalValue::LinkOnceODRLinkage: return 11;
    case GlobalValue::WeakODRLinkage: return 12;
    case GlobalValue::ExternalLinkage: return 13;
    case GlobalValue::DLLImportLinkage: return 14;
    case GlobalValue::DLLExportLinkage: return 15;
    default: exit(1);
    }

    return id;
  }   

  int id_visibility(GlobalValue::VisibilityTypes v) {
    switch (v) {
    case GlobalValue::DefaultVisibility: return 0;
    case GlobalValue::HiddenVisibility: return 1;
    case GlobalValue::ProtectedVisibility: return 2;
    default: exit(1);
    }
  }

  int id_predicate(int predicate) {
    if (predicate >= CmpInst::FIRST_FCMP_PREDICATE && predicate <= CmpInst::LAST_FCMP_PREDICATE)
      return predicate;
    else return predicate - 32;
  }

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
	if (isa<SequentialType>(T) && !isa<ArrayType>(T)) {
	  (*typeMap)[T] = depth;
	  typ = caml_alloc(1,T->getTypeID() - 9); 
	  arg = convert_aux(cast<SequentialType>(T)->getElementType(),depth+1,typeMap);
	  Store_field(typ,0,arg);
	  (*typeMap)[T] = -1;
	}
	if (isa<ArrayType>(T)) {
	  (*typeMap)[T] = depth;
	  typ = caml_alloc(2,T->getTypeID() - 9); 
	  arg = convert_aux(cast<SequentialType>(T)->getElementType(),depth+1,typeMap);
	  Store_field(typ,0,caml_copy_int32(cast<ArrayType>(T)->getNumElements()));
	  Store_field(typ,1,arg);
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

  value convert(const Constant * C);

  value convert(std::list<const Constant *>::const_iterator P) {
    CAMLparam0();
    CAMLlocal1(v);

    const Constant * C = *P;
    v = convert(C);

    CAMLreturn(v);
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
      std::list<const Constant *> l;
      for (unsigned i = 0; i < C->getNumOperands(); ++i) {
	l.push_back(cast<Constant>(C->getOperand(i)));
      }
      
      Store_field(constant,0,convertIT<std::list<const Constant *>::const_iterator>(l.begin(),l.end()));
      l.clear();
    }
    if (isa<ConstantStruct>(C) && (ok = true)) {
      constant = caml_alloc(1,2);
      std::list<const Constant *> l;
      for (unsigned i = 0; i < C->getNumOperands(); ++i) {
	l.push_back(cast<Constant>(C->getOperand(i)));
      }
      
      Store_field(constant,0,convertIT<std::list<const Constant *>::const_iterator>(l.begin(),l.end()));
      l.clear();
    }
    if (isa<ConstantArray>(C) && (ok = true)) {
      constant = caml_alloc(1,3);
      std::list<const Constant *> l;
      for (unsigned i = 0; i < C->getNumOperands(); ++i) {
	l.push_back(cast<Constant>(C->getOperand(i)));
      }
      
      Store_field(constant,0,convertIT<std::list<const Constant *>::const_iterator>(l.begin(),l.end()));
      l.clear();
    }
    if (isa<ConstantExpr>(C)) {
      const ConstantExpr *CO = cast<ConstantExpr>(C);
      if (CO->isCast()) {
	ok = true;
	constant = caml_alloc(4,10);
	Store_field(constant,0,Val_int(CO->getOpcode() - 30));
	Store_field(constant,1,convert(CO->getOperand(0)->getType()));
	Store_field(constant,2,convert(CO->getOperand(0)));
	Store_field(constant,3,convert(CO->getType())); 
      }
      switch (CO->getOpcode()) {
      case Instruction::GetElementPtr: 
	ok = true;
	constant = caml_alloc(2,9);
	const Constant * CO = cast<Constant>(C->getOperand(0));
	Store_field(constant,0,convert(CO));
	// BOGUS
	Store_field(constant,1,convertIT<User::const_op_iterator>(CO->op_begin(),CO->op_end()));
      }
    }
 
    check(ok,"constant");
    CAMLreturn(constant);
  }
  
  value convert(const Value *V) {
    CAMLparam0();
    CAMLlocal1(user);

    bool ok = false;
    user = Val_int(0);
    // Get the variable correspong to the instruction
    if (isa<Instruction>(V) && (ok = true)) {
      user = caml_alloc(1,1);
      Store_field(user,0,caml_copy_string(instNames.get(cast<const Instruction>(V)).c_str()));
    }      
    // convert the constant
    if (isa<Constant>(V) && (ok = true)) {
      user = caml_alloc(1,0);
      Store_field(user,0,convert(cast<Constant>(V)));
    }
    // Treament of operands that are function arguments
    if (isa<Argument>(V) && (ok = true)) {
      user = caml_alloc(1,1);
      std::string s = cast<Argument>(V)->getNameStr();
      Store_field(user,0,caml_copy_string(s.c_str()));
    }
    
    check(ok,"value");
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
  
  value convert(const BinaryOperator *B) {
    CAMLparam0();
    CAMLlocal1(op);

    bool b;
    int wrap = 0;
    switch (B->getOpcode()) {
    case Instruction::Add: 
    case Instruction::Sub:
    case Instruction::Mul:
    case Instruction::Shl:
      op = caml_alloc(1,id_opcode(B->getOpcode()));
      if (B->hasNoUnsignedWrap()) 
	if (B->hasNoSignedWrap()) wrap = 3;
	else wrap = 2;
      else if (B->hasNoSignedWrap()) wrap = 1;
      Store_field(op,0,Val_int(wrap)); 
      break;
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::LShr:
    case Instruction::AShr:
      op = caml_alloc(1,id_opcode(B->getOpcode()));
      b = false;
      if (B->isExact()) b = true;
      Store_field(op,0,b?Val_int(1):Val_int(0));
      break;
    default: 
      op = Val_int(id_opcode(B->getOpcode()));
    }

    CAMLreturn(op);
  }

  value mkBinInstruction(const Instruction *I) {
    CAMLparam0();
    CAMLlocal1(inst);

    inst = caml_alloc(5,5);
    Store_field(inst,0,caml_copy_string(instNames.get(I).c_str()));
    Store_field(inst,1,convert(cast<BinaryOperator>(I))); 
    Store_field(inst,2,convert(I->getType()));
    Store_field(inst,3,mkTop(I->getOperand(0))); 
    Store_field(inst,4,mkTop(I->getOperand(1)));

    CAMLreturn(inst);
  }

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

  // Conversion of an instruction to a value of type 'instruction'
  value convert(const Instruction *I) {
    CAMLparam0();
    CAMLlocal2(inst,lv); 

    bool ok = false;
    std::string var;
    //errs() << *I << "\n";
    if (I->hasName()) {
      instNames.assign(I,I->getName());
      var = instNames.get(I);
    }
    else if (!I->getType()->isVoidTy()) {
      instNames.assign(I);
      var = instNames.get(I);
    }
    inst = Val_int(0);

    // Binary operators
    if (I->isBinaryOp() && (ok = true)) 
      inst = mkBinInstruction(I);

    // Load from memory
    if (isa<LoadInst>(I) && (ok = true)) {
      inst = caml_alloc(4,7);
      const LoadInst *L = cast<LoadInst>(I);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,L->isVolatile()? Val_int(1):Val_int(0));
      Store_field(inst,2,mkTop(L->getPointerOperand()));
      Store_field(inst,3,caml_copy_int32(L->getAlignment()));
    }

    // Allocation of local variable
    if (isa<AllocaInst>(I) && (ok = true)) {
      inst = caml_alloc(4,6);
      const AllocaInst *A = cast<AllocaInst>(I);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(A->getAllocatedType()));
      Store_field(inst,2,mkSome(mkTop(A->getArraySize()))); // Meh... 
      Store_field(inst,3,caml_copy_int32(A->getAlignment()));
    }

    // Store to memory
    if (isa<StoreInst>(I) && (ok = true)) {
      inst = caml_alloc(4,8);
      const StoreInst *S = cast<StoreInst>(I);
      Store_field(inst,0,S->isVolatile()? Val_int(1):Val_int(0));
      Store_field(inst,1,mkTop(S->getValueOperand()));
      Store_field(inst,2,mkTop(S->getPointerOperand()));
      Store_field(inst,3,caml_copy_int32(S->getAlignment()));
    }

    // Return 
    if (isa<ReturnInst>(I) && (ok = true)) {
      const ReturnInst *R = cast<ReturnInst>(I);
      inst = caml_alloc(1,0);
      Store_field(inst,0,(R->getReturnValue() == NULL)? Val_int(0): mkSome(mkTop(R->getReturnValue())));
    }

    // Branch
    if (isa<BranchInst>(I) && (ok = true)) {
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

    // Comparison instruction
    if (isa<CmpInst>(I) && (ok = true)) {
      int code;
      const CmpInst *C = cast<CmpInst>(I);
      if (isa<ICmpInst>(I)) code = 11;
      else code = 12;
      inst = caml_alloc(5,code);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,Val_int(id_predicate(C->getPredicate())));
      Store_field(inst,2,convert(C->getType()));
      Store_field(inst,3,mkTop(C->getOperand(0)));
      Store_field(inst,4,mkTop(C->getOperand(1)));
    }
    // Unreachable instruction
    if (isa<UnreachableInst>(I) && (ok = true)) inst = Val_int(1);
    // Stack unwind
    if (isa<UnwindInst>(I) && (ok = true)) inst = Val_int(0);

    // Get a value from a vector
    if (isa<ExtractElementInst>(I) && (ok = true)) {
      const ExtractElementInst *E = cast<ExtractElementInst>(I);
      inst = caml_alloc(3,15);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getVectorOperand()));
      Store_field(inst,2,convert(E->getIndexOperand()));
    }

    // Insert a value into a vector
    if (isa<InsertElementInst>(I) && (ok = true)) {
      const InsertElementInst *E = cast<InsertElementInst>(I);
      inst = caml_alloc(4,16);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getOperand(0)));
      Store_field(inst,2,mkTop(E->getOperand(1)));
      Store_field(inst,3,convert(E->getOperand(2)));
    }

    // Shuffling of vector
    if (isa<ShuffleVectorInst>(I) && (ok = true)) {
      const ShuffleVectorInst *E = cast<ShuffleVectorInst>(I);
      inst = caml_alloc(4,17);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(E->getOperand(0)));
      Store_field(inst,2,mkTop(E->getOperand(1)));
      Store_field(inst,3,mkTop(E->getOperand(2)));
    }

    // Phi node
    if (isa<PHINode>(I) && (ok = true)) {
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

    // Casts
    if (isa<CastInst>(I) && (ok = true)) {
      const CastInst *C = cast<CastInst>(I);
      inst = caml_alloc(4,10);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,Val_int(C->getOpcode() - 30));
      Store_field(inst,2,mkTop(C->getOperand(0)));
      Store_field(inst,3,convert(C->getDestTy()));
    }

    // Get a value from a struct or an array
    if (isa<ExtractValueInst>(I) && (ok = true)) {
      const ExtractValueInst *E = cast<ExtractValueInst>(I);
      inst = caml_alloc(3,18);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(E->getAggregateOperand()));
      lv = convertIT<ExtractValueInst::idx_iterator>(E->idx_begin(),E->idx_end()); 
      Store_field(inst,2,lv);
    }

    // insert a vlue into a struct or an array
    if (isa<InsertValueInst>(I) && (ok = true)) {
      const InsertValueInst *E = cast<InsertValueInst>(I);
      inst = caml_alloc(4,19);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,convert(E->getAggregateOperand()));
      Store_field(inst,2,convert(E->getInsertedValueOperand()));
      lv = convertIT<InsertValueInst::idx_iterator>(E->idx_begin(),E->idx_end());
      Store_field(inst,3,lv);
    }

    // Switch
    if (isa<SwitchInst>(I)) inst = caml_alloc(4,2);
    //       { 
    //       const SwitchInst *S = cast<SwitchInst>(I);
    //       inst = caml_alloc(4,2);
    //       Store_field(inst,0,caml_copy_string(var.c_str()));
    //       Store_field(inst,1,mkTop(S->getCondition()));
    //       Store_field(inst,2,caml_copy_string(blockNames.get(S->getDefaultDest())));
    //       Store_field(inst,3,);
    //     }
    // Invoke an exception
    if (isa<InvokeInst>(I)) inst = caml_alloc(8,4);

    // Forging a pointer 
    if (isa<GetElementPtrInst>(I) && (ok = true)) { 
      const GetElementPtrInst *G = cast<GetElementPtrInst>(I);
      inst = caml_alloc(4,9);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,G->isInBounds()?Val_int(1):Val_int(0));
      Store_field(inst,2,mkTop(G->getPointerOperand()));
      lv = convertIT<User::const_op_iterator>(G->idx_begin(),G->idx_end());      
      Store_field(inst,3,lv);
    }

    // Value selection
    if (isa<SelectInst>(I) && (ok = true)) {
      const SelectInst *S = cast<SelectInst>(I);
      inst = caml_alloc(4,14);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,mkTop(S->getCondition()));
      Store_field(inst,2,mkTop(S->getTrueValue()));
      Store_field(inst,3,mkTop(S->getFalseValue()));
    }

    // Variable argument function call
    if (isa<VAArgInst>(I)) inst = caml_alloc(1,21);

    // Function call (including intrinsics)
    if (isa<CallInst>(I) && (ok = true))  {
      const CallInst *C = cast<CallInst>(I);
      inst = caml_alloc(8,20);
      Store_field(inst,0,caml_copy_string(var.c_str()));
      Store_field(inst,1,C->isTailCall()?Val_int(1):Val_int(0));
      Store_field(inst,2,Val_int(id_cc (C->getCallingConv())));
      Store_field(inst,3,Val_int(0)); // NIY
      Store_field(inst,4,convert(C->getType()));
      Store_field(inst,5,mkTop(C->getCalledValue()));
      // The -1 that follows is important. The last value in the user array is the function itself.
      lv = convertIT<User::const_op_iterator>(C->op_begin(),C->op_end()-1);
      Store_field(inst,6,lv); 
      Store_field(inst,7,Val_int(0)); // NIY
    }

    // Indirect branch to a label
    if (isa<IndirectBrInst>(I) && (ok = true)) {
      inst = caml_alloc(2,3);
      const IndirectBrInst *B = cast<IndirectBrInst>(I);
      Store_field(inst,0,mkTop(B->getAddress()));
      Store_field(inst,1,convertIT<>(B->getDestination(0),B->getDestination(0)+B->getNumDestinations()));
    } // This last store field is wacky!!!

    var.clear();

    check(ok,"instruction");
    CAMLreturn(inst);
  }

  // Conversion of a basic block to a value of type 'basicBlock'
  value convert(const BasicBlock *B) {
    CAMLparam0();
    CAMLlocal1(block);

    block = caml_alloc(2,0);
    Store_field(block,0,caml_copy_string(blockNames.get(B).c_str()));
    Store_field(block,1,convertIT<BasicBlock::const_iterator>(B->begin(),B->end()));

    CAMLreturn(block);
  }



  // I need to choose whether alignment should have an option type or not...
  value convert(const GlobalValue *GV) {
    CAMLparam0();
    CAMLlocal1(v);

    v = caml_alloc(4,0);
    // linkage
    Store_field(v,0,Val_int(id_linkage(GV->getLinkage())));
    // visibility
    Store_field(v,1,Val_int(id_visibility(GV->getVisibility())));
    // alignment
    Store_field(v,2,caml_copy_int32(GV->getAlignment()));
    // section
    Store_field(v,3,caml_copy_string(GV->getSection().c_str()));

    CAMLreturn(v);
  }
  
  // Conversion of a function to a value of type 'func'
  value convert (const Function *F) {
    CAMLparam0();
    CAMLlocal1(f);

    caml_callback(*caml_named_value("clean"),Val_int(0));
    errs() << "Converting " << F->getNameStr() << "...";
    f = caml_alloc(9,0);
    instNames.clear();

    // Assign names to blocks
    blockNames.clear();
    for (Function::const_iterator I = F->getBasicBlockList().begin(), E = F->getBasicBlockList().end(); I != E; ++I) 
      blockNames.assign(I); 

    // Function name
    Store_field(f,0,caml_copy_string(F->getNameStr().c_str()));
    // Function arguments
    Store_field(f,4,convertIT<Function::const_arg_iterator>(F->arg_begin(),F->arg_end()));
    // Function body
    Store_field(f,7,convertIT<Function::const_iterator>(F->begin(),F->end()));

    errs() << "OK\n";
    CAMLreturn(f);    
  }
  
  // Conversion of a type symbol table entry to a value of type 'namedtype'
  value convert(TypeSymbolTable::const_iterator TI) {
    CAMLparam0();
    CAMLlocal1(entry);

    entry = caml_alloc(2,0);
    // Type name
    Store_field(entry,0,caml_copy_string(TI->first.c_str()));
    // Communicating the name of the named type being computed to convert through 
    // the global variable
    converted = TI->first;
    // type 
    Store_field(entry,1,convert(TI->second));

    CAMLreturn(entry);
  }

  // Conversion of a global variable to a value of type 'global'
  value convert(const GlobalVariable *GV) {
    CAMLparam0();
    CAMLlocal1(global);

    global = caml_alloc(7,0);
    // Global variable identifier
    Store_field(global,0,caml_copy_string(GV->getNameStr().c_str()));
    // Global variable type
    Store_field(global,1,convert(GV->getType()->getElementType()));
    // Initializer, if any
    Store_field(global,2,GV->hasInitializer()? mkSome(convert(GV->getInitializer())):Val_int(0));
    // Thread locality
    Store_field(global,3,GV->isThreadLocal()? Val_int(1): Val_int(0));
    // global variable or global constant?
    Store_field(global,4,GV->isConstant()? Val_int(1):Val_int(0));
    // information
    Store_field(global,5,convert(cast<GlobalValue>(GV)));
    // Address space
    if (unsigned AddressSpace = GV->getType()->getAddressSpace()) {
      if (GV->hasUnnamedAddr()) Store_field(global,6,mkSome(caml_copy_string("addrspace(unnamed_addr)")));
      else {
	std::ostringstream out;
	out << "addrspace(" << AddressSpace << ")";
	Store_field(global,6,mkSome(caml_copy_string(out.str().c_str())));
      }
    }
    else Store_field(global,6,Val_int(0));

    CAMLreturn(global);
  }

  // Conversion of an alias to a value of type 'alias'
  // The constant, of course, has a type. Maybe that should be a top... 
  value convert(const GlobalAlias *GA) {
    CAMLparam0();
    CAMLlocal1(alias);

    alias = caml_alloc(4,0);
    // name
    Store_field(alias,0,caml_copy_string(GA->getNameStr().c_str()));
    // aliasee type
    Store_field(alias,1,convert(GA->getAliasee()->getType()));
    // aliasee 
    Store_field(alias,2,convert(GA->getAliasee()));
    // info
    Store_field(alias,3,convert(cast<GlobalValue>(GA)));

    CAMLreturn(alias);
  }

  // Conversion of a module to a value of type 'modul'
  value convert(const Module *M) {
    CAMLparam0();
    CAMLlocal1(module);
    
    module = caml_alloc(7,0);
    // Module name
    Store_field(module,0,caml_copy_string(M->getModuleIdentifier().c_str()));    
    // Data layout
    Store_field(module,1,caml_copy_string(M->getDataLayout().c_str()));
    // Platform description
    Store_field(module,2,caml_copy_string(M->getTargetTriple().c_str()));
    // Global variables
    Store_field(module,3,convertIT<Module::const_global_iterator>(M->global_begin(),M->global_end()));
    // Functions
    Store_field(module,4,convertIT<Module::const_iterator>(M->begin(),M->end()));
    // Named types
    Store_field(module,7,convertIT<TypeSymbolTable::const_iterator>(M->getTypeSymbolTable().begin(),M->getTypeSymbolTable().end()));   

    CAMLreturn(module);
  }

  typedef std::vector<const Type *> typevector;
  typedef std::map<std::string,const Type *> typefinder;

  const Type * buildType(value v, LLVMContext &C, typefinder & f); 

  typevector buildTypeVector(value v, LLVMContext &C, typefinder & f) { 
    CAMLparam1(v);

    typevector l;
    if (Is_block(v)) {
      const Type * t = buildType(Field(v,0),C,f);
      typevector l = buildTypeVector(Field(v,1),C,f);
      l.insert(l.begin(),t);
    }    
    
    CAMLreturnT(typevector,l);
  }

  const Type * buildType(value v, LLVMContext &C, typefinder &f) {
    CAMLparam1(v);

    typevector *A;

    const Type * t;
    if (Is_long(v)) t = Type::getPrimitiveType(C,(Type::TypeID) Int_val(v));
    else {
      switch (Tag_val(v)) {
      case 0: t = IntegerType::get(C,Int32_val(Field(v,0))); break;
      case 1: 
	A = new typevector(buildTypeVector(Field(v,1),C,f)); 
	t = FunctionType::get(buildType(Field(v,0),C,f),A); break; // LOI
      case 2: t = StructType::get(C,buildTypeVector(Field(v,0),C,f)); break; // LOI
      case 3: t = ArrayType::get(buildType(Field(v,1),C,f),Int32_val(Field(v,0))); break;
      case 4: t = PointerType::getUnqual(buildType(Field(v,0),C,f)); break; // LOI
      case 6: t = f[std::string(String_val(Field(v,0)))];
      default: exit(1);
      }
    }

    CAMLreturnT(const Type *,t);
  }

  Module * buildModule(value v) {
    CAMLparam1(v);
    
    if (!Is_block(v)) exit(1);
    StringRef s = StringRef(String_val(Field(v,0)));
    LLVMContext context;
    Module * m = new Module(s,context);

    CAMLreturnT(Module *,m);
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
