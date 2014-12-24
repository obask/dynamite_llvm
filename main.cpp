#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include <cctype>
#include <cstdio>
#include <map>
#include <string>
#include <vector>


#include "codegen.hpp"
#include "ast.hpp"
#include "parser.hpp"

using namespace llvm;


//===----------------------------------------------------------------------===//
// Top-Level parsing and JIT Driver
//===----------------------------------------------------------------------===//

static ExecutionEngine *TheExecutionEngine;

static void HandleDefinition() {
  if (FunctionAST *F = ParseDefinition()) {
    if (Function *LF = F->Codegen()) {
      fprintf(stderr, "Read function definition:");
      LF->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleExtern() {
  if (PrototypeAST *P = ParseExtern()) {
    if (Function *F = P->Codegen()) {
      fprintf(stderr, "Read extern: ");
      F->dump();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (FunctionAST *F = ParseTopLevelExpr()) {
    if (Function *LF = F->Codegen()) {
      TheExecutionEngine->finalizeObject();
      // JIT the function, returning a function pointer.
      void *FPtr = TheExecutionEngine->getPointerToFunction(LF);

      // Cast it to the right type (takes no arguments, returns a double) so we
      // can call it as a native function.
      double (*FP)() = (double (*)())(intptr_t)FPtr;
      fprintf(stderr, "Evaluated to %f\n", FP());
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (1) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';':
      getNextToken();
      break; // ignore top-level semicolons.
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}






/// prototype
///   ::= id '(' id* ')'
static PrototypeAST *ParsePrototype2() {
  std::string FnName = "plus";
//  std::vector<std::string> ArgNames = {"a", "b"};
  return new PrototypeAST("plus",  {"a", "b"});
}

/// expression
///   ::= primary binoprhs
///
static ExprAST *ParseExpression2() {

  ExprAST *e1 = new NumberExprAST(124);
  ExprAST *e2 = new VariableExprAST("a");
  ExprAST *e3 = new VariableExprAST("b");
  ExprAST *e4 = new CallExprAST("minus", {e1, e2});
  ExprAST *e5 = new BinaryExprAST('+', e1, e2);

//  case tok_identifier: return ParseIdentifierExpr();

//    new VariableExprAST("IdName");


//  case '(':            return ParseParenExpr();
  // other expr

  return e5;
}


static void HandleDefinition2() {

  PrototypeAST *Proto1 = new PrototypeAST("plus",  {"a", "b"});
  ExprAST *E1 = ParseExpression2();
  FunctionAST *F1 = new FunctionAST(Proto1, E1);

  ExprAST *E2 = new BinaryExprAST('+', new NumberExprAST(5), new NumberExprAST(2));
  FunctionAST *F2 = new FunctionAST(new PrototypeAST("consta",  {}), E2);

  fprintf(stderr, "\n");

  if (Function *LF = F1->Codegen()) {
    if (Function *LF2 = F2->Codegen()) {
      fprintf(stderr, "Read function definition:");
//      LF->dump();
    }
  }

}

// BAD factorial

//ExprAST *cond0 = new BinaryExprAST('<', new VariableExprAST("n"), new NumberExprAST(2));
//ExprAST *then0 = new BinaryExprAST('=', new VariableExprAST("rez"), new NumberExprAST(1));
//
//
//ExprAST *fac0 = new CallExprAST("fac", {new BinaryExprAST('-', new VariableExprAST("n"),
//        new NumberExprAST(1))});
//
//ExprAST *else0 = new BinaryExprAST('=', new VariableExprAST("rez"),
//        new BinaryExprAST('*', new VariableExprAST("n"), fac0));
//
//ExprAST *ex = new IfExprAST(cond0, then0, else0);
//
//ExprAST *tmp = new CallExprAST("seq", {ex, new VariableExprAST("rez")});
//
//ExprAST *e4 = new LetExprAST(std::make_pair("rez", new NumberExprAST(275)), tmp);



static void HandleFucktorial() {
  PrototypeAST *seqProto = new PrototypeAST("seq",  {"a", "b"});
  FunctionAST *seqFun = new FunctionAST(seqProto, new VariableExprAST("b"));
  if (Function *LF0 = seqFun->Codegen()) {
    fprintf(stderr, "Read function definition: %s\n", LF0->getName().data());
  }

//  PrototypeAST *Proto1 = new PrototypeAST("fac",  {"n"});

  ExprAST *cond0 = new BinaryExprAST('<', new VariableExprAST("n"), new NumberExprAST(2));
  ExprAST *else0 = new BinaryExprAST('*', new VariableExprAST("n"),
                                          new CallExprAST("fac", {new BinaryExprAST('-', new VariableExprAST("n"),
                                                                                        new NumberExprAST(1))}));

//  ExprAST *resIf = new IfExprAST(cond0, new NumberExprAST(1), else0);


  PrototypeAST *Proto1 = new PrototypeAST("fac",  {"n"});
  ExprAST *resIf = new IfExprAST(new BinaryExprAST('<', new VariableExprAST("n"), new NumberExprAST(2)),
                                 new NumberExprAST(1),
                                 new BinaryExprAST('*', new VariableExprAST("n"),
                                                    new CallExprAST("fac", {new BinaryExprAST('-', new VariableExprAST("n"),
                                                            new NumberExprAST(1))})));
  FunctionAST *F1 = new FunctionAST(Proto1, resIf);

  if (Function *LF = F1->Codegen()) {
    fprintf(stderr, "Read function definition: %s\n", LF->getName().data());
  }


  PrototypeAST *replProto = new PrototypeAST("repl",  {});
  FunctionAST *replFun = new FunctionAST(replProto, new CallExprAST("fac", {new NumberExprAST(6)}));
  if (Function *LFrepl = replFun->Codegen()) {
    fprintf(stderr, "Read function definition: %s\n", LFrepl->getName().data());

    TheExecutionEngine->finalizeObject();
    // JIT the function, returning a function pointer.
    void *FPtr = TheExecutionEngine->getPointerToFunction(LFrepl);

    // Cast it to the right type (takes no arguments, returns a double) so we
    // can call it as a native function.
    double (*FP)() = (double (*)())(intptr_t)FPtr;
    fprintf(stderr, "Evaluated to %f\n", FP());
  }

  fprintf(stderr, "\n");

}


static void HandleStruct() {

  PrototypeAST *replProto = new PrototypeAST("repl",  {});
  FunctionAST *replFun = new FunctionAST(replProto, new CallExprAST("fac", {new NumberExprAST(6)}));

  std::vector<Type *> Doubles(2, Type::getDoubleTy(getGlobalContext()));

  Type* doubleType = Type::getDoubleTy(getGlobalContext());

  StructType *structType = StructType::get(doubleType, doubleType, doubleType, NULL);

  LLVMContext &C = getGlobalContext();

  ConstantInt *val_mem = ConstantInt::get(C, APInt(32, 1));



  BasicBlock* BB = Builder.GetInsertBlock();
  Type* IntPtrTy = IntegerType::getInt32Ty(C);
  Type* Int8Ty = IntegerType::getInt8Ty(C);
  Constant* allocsize = ConstantExpr::getSizeOf(Int8Ty);
  allocsize = ConstantExpr::getTruncOrBitCast(allocsize, IntPtrTy);

  Instruction* ptr_arr = CallInst::CreateMalloc(BB, IntPtrTy, Int8Ty, allocsize);

  BB->getInstList().push_back(cast<Instruction>(ptr_arr));



  if (Function *LFrepl = replFun->Codegen()) {
    fprintf(stderr, "Read function definition: %s\n", LFrepl->getName().data());

    TheExecutionEngine->finalizeObject();
    // JIT the function, returning a function pointer.
    void *FPtr = TheExecutionEngine->getPointerToFunction(LFrepl);

    // Cast it to the right type (takes no arguments, returns a double) so we
    // can call it as a native function.
    double (*FP)() = (double (*)())(intptr_t)FPtr;
    fprintf(stderr, "Evaluated to %f\n", FP());
  }

  fprintf(stderr, "\n");

}

static void handleMalloc() {
  LLVMContext &C = getGlobalContext();
  FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), {}, false);
  Function *repl = Function::Create(FT, Function::ExternalLinkage, "REPL", TheModule);

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(C, "entry", repl);
  Builder.SetInsertPoint(BB);

  Value *RetVal = ConstantFP::get(C, APFloat(7.0));

  Type* IntPtrTy = IntegerType::getInt32Ty(C);
  Type* Int8Ty = IntegerType::getDoubleTy(C);
  Constant* allocsize = ConstantExpr::getSizeOf(Int8Ty);
  allocsize = ConstantExpr::getTruncOrBitCast(allocsize, IntPtrTy);

  Instruction* ptr_arr = CallInst::CreateMalloc(BB, IntPtrTy, Int8Ty, allocsize);

  Value *t1 = Builder.Insert(ptr_arr);

  Value *mydata = Builder.CreateAlloca(Type::getDoublePtrTy(C));

  Builder.CreateStore(t1, mydata);



  // Finish off the function.
  Builder.CreateRet(RetVal);

  // Validate the generated code, checking for consistency.
  verifyFunction(*repl);

  // Optimize the function.
//  TheFPM->run(*repl);

  TheExecutionEngine->finalizeObject();
  // JIT the function, returning a function pointer.
  void *FPtr = TheExecutionEngine->getPointerToFunction(repl);
  // Cast it to the right type (takes no arguments, returns a double) so we
  // can call it as a native function.
  double (*FP)() = (double (*)())(intptr_t)FPtr;
  fprintf(stderr, "Evaluated to %f\n", FP());

}



static void handleStruct() {
  LLVMContext &C = getGlobalContext();

  GlobalVariable* gvar_int32_xx = new GlobalVariable(/*Module=*/*TheModule,
          /*Type=*/IntegerType::getInt32Ty(C),
          /*isConstant=*/true,
          /*Linkage=*/GlobalValue::ExternalLinkage,
          /*Initializer=*/0, // has initializer, specified below
          /*Name=*/"xx");
  gvar_int32_xx->setAlignment(4);

  ConstantInt* const_int32_7 = ConstantInt::get(C, APInt(32, 1234));
  gvar_int32_xx->setInitializer(const_int32_7);



  std::vector<Type*> tt = {Type::getInt32Ty(C)};
  StructType *StructTy_Object = StructType::create(tt, "struct.object");

  StructType *StructTy_struct_list = TheModule->getTypeByName("struct.list");
  if (!StructTy_struct_list) {
    StructTy_struct_list = StructType::create(C, "struct.list");
  }
  PointerType* PointerTy_struct_list = PointerType::getUnqual(StructTy_struct_list);
  if (StructTy_struct_list->isOpaque()) {
    StructTy_struct_list->setBody(StructTy_Object, PointerTy_struct_list, Type::getInt32Ty(C), NULL);
  }


  FunctionType *FT = FunctionType::get(Type::getDoubleTy(C), {}, false);
  Function *repl = Function::Create(FT, Function::ExternalLinkage, "REPL", TheModule);

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(C, "entry", repl);
  Builder.SetInsertPoint(BB);

  AllocaInst* ptr_ptr1 = Builder.CreateAlloca(PointerTy_struct_list, 0, "ptr");

  Value *RetVal = ConstantFP::get(C, APFloat(7.0));

  Type* IntPtrTy = IntegerType::getInt32Ty(C);
  Type* Int8Ty = IntegerType::getDoubleTy(C);
  Constant* allocsize = ConstantExpr::getSizeOf(Int8Ty);
  allocsize = ConstantExpr::getTruncOrBitCast(allocsize, IntPtrTy);

  Instruction* ptr_arr = CallInst::CreateMalloc(BB, IntPtrTy, Int8Ty, allocsize);

  Value *t1 = Builder.Insert(ptr_arr);

  Value *mydata = Builder.CreateAlloca(Type::getDoublePtrTy(C));

  Builder.CreateStore(t1, mydata);



  // Finish off the function.
  Builder.CreateRet(RetVal);

  // Validate the generated code, checking for consistency.
  verifyFunction(*repl);

  // Optimize the function.
//  TheFPM->run(*repl);

  TheExecutionEngine->finalizeObject();
  // JIT the function, returning a function pointer.
  void *FPtr = TheExecutionEngine->getPointerToFunction(repl);
  // Cast it to the right type (takes no arguments, returns a double) so we
  // can call it as a native function.
  double (*FP)() = (double (*)())(intptr_t)FPtr;
  fprintf(stderr, "Evaluated to %f\n", FP());

}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

/// putchard - putchar that takes a double and returns 0.
extern "C" double putchard(double X) {
  putchar((char)X);
  return 0;
}

/// printd - printf that takes a double prints it as "%f\n", returning 0.
extern "C" double printd(double X) {
  printf("%f\n", X);
  return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  LLVMContext &Context = getGlobalContext();

  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['='] = 2;
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40; // highest.

  // Prime the first token.
  //  fprintf(stderr, "ready> ");
  //  getNextToken();

  // Make the module, which holds all the code.
  std::unique_ptr<Module> Owner = make_unique<Module>("my cool jit", Context);
  TheModule = Owner.get();

  // Create the JIT.  This takes ownership of the module.
  std::string ErrStr;
  TheExecutionEngine =
      EngineBuilder(std::move(Owner))
          .setErrorStr(&ErrStr)
          .setMCJITMemoryManager(llvm::make_unique<SectionMemoryManager>())
          .create();
  if (!TheExecutionEngine) {
    fprintf(stderr, "Could not create ExecutionEngine: %s\n", ErrStr.c_str());
    exit(1);
  }

  FunctionPassManager OurFPM(TheModule);

  // Set up the optimizer pipeline.  Start with registering info about how the
  // target lays out data structures.
  TheModule->setDataLayout(TheExecutionEngine->getDataLayout());
  OurFPM.add(new DataLayoutPass());
  // Provide basic AliasAnalysis support for GVN.
  OurFPM.add(createBasicAliasAnalysisPass());
  // Promote allocas to registers.
  OurFPM.add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  OurFPM.add(createInstructionCombiningPass());
  // Reassociate expressions.
  OurFPM.add(createReassociatePass());
  // Eliminate Common SubExpressions.
  OurFPM.add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  OurFPM.add(createCFGSimplificationPass());

  OurFPM.doInitialization();

  // Set the global so the code gen can use this.
  TheFPM = &OurFPM;

  // Run the main "interpreter loop" now.
//  MainLoop();

  // Run the main "interpreter loop" now.
  handleStruct();


  TheFPM = 0;

  // Print out all of the generated code.
  TheModule->dump();

  return 0;
}
