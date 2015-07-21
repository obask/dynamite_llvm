#ifndef HANDLE_AST
#define HANDLE_AST

#include "globals.cpp"
#include "default_types.cpp"
#include "extern_lib.cpp"

#include <iostream>
#include <unordered_set>

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
#include "llvm/IR/Value.h"
#include "llvm/PassManager.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include "ltoken.hpp"
#include "SyntaxTree.hpp"
#include "utilz.hpp"


Value *handleCall(string Op, vector<SyntaxTreeP> args);

Value *handleSelectApply(string Op, vector<SyntaxTreeP> args);

Value *handleApply(string oper, vector<SyntaxTreeP> args);

Value *internalCall(string Op, vector<Value*> calc_args);

void handleIR(SyntaxTreeP tree);

Value *handleValue(SyntaxTreeP tt);


Value *handleIfExpr(SyntaxTreeP cond, SyntaxTreeP thenBranch, SyntaxTreeP elseBranch) {
    LLVMContext &C = getGlobalContext();

    cout << "handleIfExpr: " << cond->toString() << endl;

    Value *condVal0 = handleValue(cond);
    if (condVal0 == 0)
        return 0;

    condVal0->dump();

    // Convert condition to a bool by comparing equal to 0.0.
    Value *condVal = Builder.CreateICmpNE(condVal0, ConstantInt::get(C, APInt(32, 0)), "ifcond");

    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the then and else cases.  Insert the 'then' block at the
    // end of the function.
    BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
    BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

    Builder.CreateCondBr(condVal, ThenBB, ElseBB);

    // Emit then value.
    Builder.SetInsertPoint(ThenBB);

    Value *thenVal = handleValue(thenBranch);
    if (thenVal == NULL)
        return NULL;

    Builder.CreateBr(MergeBB);
    // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
    ThenBB = Builder.GetInsertBlock();

    // Emit else block.
    TheFunction->getBasicBlockList().push_back(ElseBB);
    Builder.SetInsertPoint(ElseBB);

    Value *elseVal = handleValue(elseBranch);
    if (elseVal == NULL)
        return NULL;

    Builder.CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = Builder.GetInsertBlock();

    // Emit merge block.
    TheFunction->getBasicBlockList().push_back(MergeBB);
    Builder.SetInsertPoint(MergeBB);
    PHINode *PN = Builder.CreatePHI(Type::getInt32Ty(C), 2, "iftmp");

    PN->addIncoming(thenVal, ThenBB);
    PN->addIncoming(elseVal, ElseBB);
    return PN;
}

Value* handleValDef(SyntaxTreeP varName, SyntaxTreeP value) {
    string var = varName->getString();
    cout << "handleValDef: " << var << endl;
    Value *calcResult = handleValue(value);


    // save new variable
    AllocaInst* ptr_context = Builder.CreateAlloca(calcResult->getType());
    ptr_context->setAlignment(8);
    // Add arguments to variable symbol table.
    // FIXME should shadow up to stack vars
    NamedValues[var] = ptr_context;

    // Store the initial value into the alloca.
    return Builder.CreateStore(calcResult, ptr_context);
}



Value *handleApply(string oper, vector<SyntaxTreeP> args) {
    // FIXME test it вызов функции или конструктора
    LLVMContext &C = getGlobalContext();

    cout << "handleApply " << oper << " : " << args.size() << endl;

    Constant *methodName = internalString(oper);

    vector<Value *> calc_args;
    calc_args.push_back(methodName);
    for (auto arg: args) {
        if (arg == NULL) {
            cout << "NULL" << endl;
            throw std::logic_error("null error");
        }
        Value *RZ = handleValue(arg);
        if (RZ) {
            calc_args.push_back(RZ);
        } else {
            throw std::logic_error("Value *handleCall(string Op, vector<SyntaxTreeP> args)");
        }
    }
    cout << "Args calculated" << endl;

    string callee = oper;

    // constructor check
    if (isupper(oper[0])) {
        if (caseClasses.count(oper)) {
            callee = "init" + oper;
        } else {
            assert(!"Case class not found!");
        }
    }

    return internalCall(callee, calc_args);
}


Value* bitCastCall(string Op, vector<Value*> calc_args) {
    Function *func = TheModule->getFunction(Op);
    assert(func && "FFI operator not found!");
    FunctionType *pType = func->getFunctionType();
    fprintf(stderr, "// bitCastCall: %s|\n", Op.c_str());
    pType->dump();
    return nullptr;
}


Value* internalCall(string Op, vector<Value*> calc_args) {
    LLVMContext &C = getGlobalContext();
    cout << "internalCall " << calc_args.size() << " " << Op << endl;

    fprintf(stderr, "ARGS:\n");
    for (int i=0; i< calc_args.size(); ++i) {
        calc_args[i]->getType()->dump();
        calc_args[i]->dump();
    }
    fprintf(stderr, "----\n");

    if (Op == "+") {
//        return Builder.CreateFAdd(calc_args[0], calc_args[1], "addtmp");
        return Builder.CreateAdd(calc_args[0], calc_args[1], "addtmp");

    }
    if (Op == "-") {
        return Builder.CreateSub(calc_args[0], calc_args[1], "subtmp");
//        return Builder.CreateFSub(calc_args[0], calc_args[1], "subtmp");
    }
    if (Op == "*") {
        return Builder.CreateMul(calc_args[0], calc_args[1], "multmp");
//        return Builder.CreateFMul(calc_args[0], calc_args[1], "multmp");
    }
    if (Op == "<") {
        Value *RZ = Builder.CreateICmpULT(calc_args[0], calc_args[1], "cmptmp");
        cout << "DBG 77" << endl;

        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder.CreateZExt(RZ, Type::getInt32Ty(C), "booltmp");
    }

    // If it wasn't a builtin binary operator, it must be a user defined one. Emit
    // a call to it.
    Function *func = TheModule->getFunction(Op);
    assert(func && "binary operator not found!");

    fprintf(stderr, "// bitCast: %s\n", Op.c_str());
    FunctionType *funType = func->getFunctionType();
//    pType->dump();
    unsigned int numParams = funType->getNumParams();
    for (unsigned i=0; i < numParams; ++i) {
        funType->getParamType(i)->dump();
    }
    fprintf(stderr, "->\n");
    Type *returnType = funType->getReturnType();
    returnType->dump();
    fprintf(stderr, "------------\n");
    vector<Value* > newArgs;
    for (int i=0; i < calc_args.size(); ++i) {
        calc_args[i]->getType()->dump();
        Value *cast1 = Builder.CreatePointerCast(calc_args[i], funType->getParamType(i));
        newArgs.push_back(cast1);
    }
    fprintf(stderr, "dumping:\n");
    for (unsigned i=0; i < newArgs.size(); ++i) {
        newArgs[i]->dump();
    }
    fprintf(stderr, "ok\n");

//    Value *Ops[] = { L, R };
    CallInst *pInst = Builder.CreateCall(func, newArgs);
    printf("OK\n");

    if (pInst->getType() == Type::getVoidTy(C)) {
        return nullptr;
    }
    // else
    PointerType *objType = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));
    return pInst;
    // Builder.CreateBitCast(pInst, objType);
}


Value *handleCall(string Op, vector<SyntaxTreeP> args) {
    LLVMContext &C = getGlobalContext();

    cout << "handleCall " << Op << " : " << args.size() << endl;

    vector<Value *> calc_args;
    for (auto arg: args) {
        if (arg == NULL) {
            cout << "NULL" << endl;
            throw std::logic_error("null error");
        }
        Value *RZ = handleValue(arg);
        if (RZ) {
            calc_args.push_back(RZ);
        } else {
            throw std::logic_error("Value *handleCall(string Op, vector<SyntaxTreeP> args)");
        }
    }
    cout << "Args calculated" << endl;

    return internalCall(Op, calc_args);
}


Value *handleSelectApply(string Op, vector<SyntaxTreeP> args) {
    LLVMContext &C = getGlobalContext();

    cout << "handleSelectApply " << Op << " : " << args.size() << endl;

    Constant *methodName = internalString(Op);

    vector<Value *> calc_args;
    calc_args.push_back(methodName);
    for (auto arg: args) {
        if (arg == NULL) {
            cout << "NULL" << endl;
            throw std::logic_error("null error");
        }
        Value *RZ = handleValue(arg);
        if (RZ) {
            calc_args.push_back(RZ);
        } else {
            throw std::logic_error("Value *handleCall(string Op, vector<SyntaxTreeP> args)");
        }
    }
    cout << "Args calculated" << endl;

    std::string callee = "callFFI" + to_string(calc_args.size() - 2);

    return internalCall(callee, calc_args);
}

Type *getTypeOfName(string typeName) {
    LLVMContext &C = getGlobalContext();
    if (typeName == "EmptyTree") {
        return Type::getVoidTy(C);
    }
    if (typeName == "String") {
        return PointerType::getUnqual(IntegerType::getInt8Ty(C));
    }
    if (typeName == "Int") {
        return IntegerType::getInt32Ty(C);
    }
    if (typeName == "Boolean") {
        return IntegerType::getInt32Ty(C);
    }
    StructType *pType = TheModule->getTypeByName("struct." + typeName);
    if (!pType) {
        pType = StructType::create(TheModule->getContext(), "struct." + typeName);
    }
    assert(pType);
    return PointerType::getUnqual(pType);
}


Type *getPtrToOpaqueTypes(SyntaxTreeP typeNameX) {
    // array types
    if (typeid(*typeNameX) == typeid(ABranch) && typeNameX->elemAt(0)->getString() == "Array") {
        auto typeName = typeNameX->elemAt(1)->getString();
        Type *typeOfName = getTypeOfName(typeName);
        return PointerType::getUnqual(typeOfName);
    }
    // else simple type
    auto typeName = typeNameX->getString();
    return getTypeOfName(typeName);
}



Function *handleSignature(const string &name, const vector<SyntaxTreeP> &fields, SyntaxTreeP returnType) {
    LLVMContext &C = getGlobalContext();
    cout << "handleSignature " << name << " -> " << returnType << endl;

    Type *PointerTy_Object = getPtrToOpaqueTypes(returnType);

    vector<string> args;

    cout << "DBG 2" << endl;

    std::vector<Type *> argsTypes;
    for (const auto &argType: fields) {
        auto tmp = getPtrToOpaqueTypes(argType);
        argsTypes.push_back(tmp);
    }

    FunctionType *FT = FunctionType::get(PointerTy_Object, argsTypes, false);
    assert(FT);

//    FunctionType *FT = FunctionType::get(Type::getInt32Ty(C), {}, false);
    Function *theFunction = Function::Create(FT, Function::ExternalLinkage, name, TheModule);
    assert(theFunction);

    cerr << "-- theFunction" << endl;
//    theFunction->dump();
    cerr << endl;
    cerr.flush();

    return theFunction;
}

Function *handleDefun(SyntaxTreeP tree) {
    LLVMContext &C = getGlobalContext();
    cout << "handleDefun " << tree->toString() << endl;

    PointerType* PointerTy_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));

    string name = tree->elemAt(1)->getString();
    vector<SyntaxTreeP> proto = tree->elemAt(2)->getVector();
    vector<string> args;

    cout << "DBG 2" << endl;

    for (SyntaxTreeP az: proto) {
        args.push_back(az->getString());
    }

    std::vector<Type *> argsTypes(args.size(), PointerTy_Object);
    FunctionType *FT = FunctionType::get(PointerTy_Object, argsTypes, false);

//    FunctionType *FT = FunctionType::get(Type::getInt32Ty(C), {}, false);
    Function *theFunction = Function::Create(FT, Function::ExternalLinkage, name, TheModule);

    cout << "DBG 3" << endl;
    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(C, "entry", theFunction);
    Builder.SetInsertPoint(BB);

    // TODO make alloca for function args
    Function::arg_iterator AI = theFunction->arg_begin();
    for (unsigned Idx = 0; Idx != args.size(); ++Idx, ++AI) {
        // Create an alloca for this variable.
        auto varName = args[Idx];
        cout << "arg: " << varName << endl;
        IRBuilder<> TmpB(&theFunction->getEntryBlock(),
                theFunction->getEntryBlock().begin());
        cout << "DBG 31" << endl;
        AllocaInst *Alloca = TmpB.CreateAlloca(PointerTy_Object, 0, varName.c_str());
        cout << "DBG 32 " << AI << endl;

        // Store the initial value into the alloca.
        Builder.CreateStore(AI, Alloca);
        cout << "DBG 33" << endl;

        // Add arguments to variable symbol table.
        NamedValues[varName] = Alloca;
        cout << "DBG 34" << endl;
    }
    cout << "DBG 4" << endl;

    vector<SyntaxTreeP> body;
    int sz = (int)tree->getVector().size();
    for (int i=3; i < sz; ++i) {
        body.push_back(tree->elemAt(i));
    }
    Value *RetVal;

    cout << "BODY:" << endl;
    for (SyntaxTreeP tmp: body) {
        cout << "-> " << tmp->toString() << endl;
    }

    for (auto xx: body) {
        RetVal = handleValue(xx);
    }

    if (RetVal) {
        // Finish off the function.
        Builder.CreateRet(RetVal);

        // Validate the generated code, checking for consistency.
        verifyFunction(*theFunction);

        // Optimize the function.

//        TheFPM->run(*theFunction);

        return theFunction;
    } else {
        return NULL;
    }
}



void handleSet(SyntaxTreeP tree) {
    LLVMContext &C = getGlobalContext();
    cout << "handleSet" << endl;

    string name = tree->elemAt(1)->getString();
    SyntaxTreeP body = tree->elemAt(2);

    FunctionType *FT = FunctionType::get(Type::getDoubleTy(C), {}, false);
    Function *repl = Function::Create(FT, Function::ExternalLinkage, "REPL", TheModule);

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(C, "entry", repl);
    Builder.SetInsertPoint(BB);
    handleValue(body);
    return;
}



void parseTypeDef(SyntaxTreeP tree) {
    cout << "parseTypeDef " << endl;
    string name = tree->elemAt(1)->getString();
    auto rawFields = tree->elemAt(2)->getVector();
    SmallVector<string, VEC_SIZE> fields;
    for (auto it : rawFields) {
        fields.push_back(it->getString());
    }
    auto rawParents = tree->elemAt(3)->getVector();
    SmallVector<string, VEC_SIZE> parents;
    for (int i = 1; i < rawParents.size(); ++i) {
        parents.push_back(rawParents.at(i)->getString());
    }
    vector<SyntaxTreeP> body;
    for (int i = 4; i < tree->getVector().size(); ++i) {
        body.push_back(tree->elemAt(i));
    }
    handleTypeDef(name, fields, parents, body);
}


void parseSignature(SyntaxTreeP tree) {
    cout << "parseSignature " << endl;
    string name = tree->elemAt(1)->getString();
    auto rawFields = tree->elemAt(2)->getVector();
    // Check bad function signature
    assert(tree->elemAt(3)->getString() == "->");
    SyntaxTreeP resultType = tree->elemAt(4);
    handleSignature(name, rawFields, resultType);
}

//; Function Attrs: nounwind ssp uwtable
//        define void @ZZZZZ(%struct.ObjectX* %AAAAA) #0 {
//%1 = alloca %struct.ObjectX*, align 8
//store %struct.ObjectX* %AAAAA, %struct.ObjectX** %1, align 8
//%2 = load %struct.ObjectX** %1, align 8
//%3 = bitcast %struct.ObjectX* %2 to i8*
//%4 = call %struct.LLVMOpaqueModule* @LLVMModuleCreateWithName(i8* %3)
//ret void
//}
//
//void genFunCallFFI() {
//
//    func_ZZZZZ = TheModule->getFunction(ZZZZZ);
//
//    Function::arg_iterator args = func_ZZZZZ->arg_begin();
//    Value* ptr_AAAAA = args++;
//    ptr_AAAAA->setName("AAAAA");
//
//    BasicBlock* label_23 = BasicBlock::Create(mod->getContext(), "",func_ZZZZZ,0);
//
//    // Block  (label_23)
//    AllocaInst* ptr_24 = new AllocaInst(PointerTy_5, "", label_23);
//    ptr_24->setAlignment(8);
//    StoreInst* void_25 = new StoreInst(ptr_AAAAA, ptr_24, false, label_23);
//    void_25->setAlignment(8);
//    LoadInst* ptr_26 = new LoadInst(ptr_24, "", false, label_23);
//    ptr_26->setAlignment(8);
//    CastInst* ptr_27 = new BitCastInst(ptr_26, PointerTy_7, "", label_23);
//    CallInst* ptr_28 = CallInst::Create(func_LLVMModuleCreateWithName, ptr_27, "", label_23);
//    ptr_28->setCallingConv(CallingConv::C);
//    ptr_28->setTailCall(false);
//    AttributeSet ptr_28_PAL;
//    ptr_28->setAttributes(ptr_28_PAL);
//
//    ReturnInst::Create(mod->getContext(), label_23);
//
//}


void handleTypeFFI(string name) {
    cout << "handleTypeFFI " << name << endl;

    StructType *StructTy_struct_LLVMOpaqueModule = TheModule->getTypeByName(("struct." + name).c_str());
    if (!StructTy_struct_LLVMOpaqueModule) {
        StructTy_struct_LLVMOpaqueModule = StructType::create(TheModule->getContext(), ("struct." + name).c_str());
    }
//    std::vector<Type*>StructTy_struct_LLVMOpaqueModule_fields;
//    if (StructTy_struct_LLVMOpaqueModule->isOpaque()) {
//        StructTy_struct_LLVMOpaqueModule->setBody(StructTy_struct_LLVMOpaqueModule_fields, /*isPacked=*/false);
//    }
    // declare as opaque type
}


Value *handleValue(SyntaxTreeP tt) {
    LLVMContext &C = getGlobalContext();
    cout << "handleValue: " << tt->toString() << endl;

    if (typeid(*tt) == typeid(ABranch) && tt->elemAt(0)->getString() == "ValDef") {
        return handleValDef(tt->elemAt(1), tt->elemAt(2));
    }

    if (typeid(*tt) == typeid(ASymbol) && tt->getString() == "null") {

        cout << "dbg null 1 " << endl;

        PointerType* PointerTy_struct_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));
        Value* res = ConstantPointerNull::get(PointerTy_struct_Object);

        cout << "dbg null 2 " << endl;

        return res;
    }

    if (typeid(*tt) == typeid(ASymbol) && tt->getString().at(0) == '\"') {
        cout << "string " << endl;
        return handleString(tt->getString());
    }

    if (typeid(*tt) == typeid(ASymbol)) {
        string var = tt->getString();
        cout << "var = " << var << endl;
        AllocaInst *Vxx = NamedValues[var];
        return Builder.CreateLoad(Vxx, var.c_str());
    }

    if (typeid(*tt) == typeid(ANumber)) {
        cout << "handleNumber" << endl;
        shared_ptr<ANumber> num = dynamic_pointer_cast<ANumber>(tt);
        return ConstantInt::get(C, APInt(32, (uint64_t) num->value));
    }

    if (typeid(*tt) == typeid(ADouble)) {
        cout << "handleDouble" << endl;
        shared_ptr<ADouble> num = dynamic_pointer_cast<ADouble>(tt);
        vector<Value*> args;
        args.push_back(ConstantFP::get(C, APFloat(num->value)));
        return internalCall("createDouble", args);
    }

    // if
    if (typeid(*tt) == typeid(ABranch)
        && typeid(*tt->elemAt(0)) == typeid(ASymbol)
        && tt->elemAt(0)->getString() == "if") {
        return handleIfExpr(tt->elemAt(1), tt->elemAt(2), tt->elemAt(3));
    }

    // SelectApply
    if (typeid(*tt) == typeid(ABranch)
        && typeid(*tt->elemAt(0)) == typeid(ASymbol)
        && tt->elemAt(0)->getString() == "SelectApply") {
        string method = tt->elemAt(1)->getString();
        int sz = (int) tt->getVector().size();
        vector<SyntaxTreeP> args;
        for (int i = 2; i < sz; ++i) {
            args.push_back(tt->elemAt(i));
        }
        return handleSelectApply(method, args);
    }

    // Apply
    if (typeid(*tt) == typeid(ABranch)
        && typeid(*tt->elemAt(0)) == typeid(ASymbol)
        && tt->elemAt(0)->getString() == "Apply") {


        string method = tt->elemAt(1)->getString();
        int sz = (int) tt->getVector().size();
        vector<SyntaxTreeP> args;
        for (int i = 2; i < sz; ++i) {
            args.push_back(tt->elemAt(i));
        }

        return handleApply(method, args);
    }

    // other ABranch
    if (typeid(*tt) == typeid(ABranch)) {
        string fun = tt->elemAt(0)->getString();
        int sz = (int) tt->getVector().size();
        vector<SyntaxTreeP> args;
        for (int i = 1; i < sz; ++i) {
            args.push_back(tt->elemAt(i));
        }
        return handleCall(fun, args);
    }

    throw std::logic_error(tt->toString());

//    return NULL;
}



void handleIR(SyntaxTreeP tree) {
    LLVMContext &C = getGlobalContext();
    cout << "handleIR " << tree->toString() << endl;

    SyntaxTreeP tmp2 = tree->elemAt(0);
    shared_ptr<ASymbol> cmd = dynamic_pointer_cast<ASymbol>(tmp2);

    if (cmd->value == "TypeDef") {
        parseTypeDef(tree);
        return;
    }

    if (cmd->value == "Signature") {
        parseSignature(tree);
        return;
    }


    if (cmd->value == "TypeFFI") {
        handleTypeFFI(tree->elemAt(1)->getString());
        return;
    }


//    if (cmd->value == "object") {
//        string name = tree->elemAt(1)->getString();
//        vector<string> args;
//        args.push_back(tree->elemAt(2)->getString());
//        args.push_back(tree->elemAt(3)->getString());
//        handleStruct(name, args);
//        return;
//    }
//
//
//    if (cmd->value == "object") {
//        string name = tree->elemAt(1)->getString();
//        vector<string> args;
//        args.push_back(tree->elemAt(2)->getString());
//        args.push_back(tree->elemAt(3)->getString());
//        handleStruct(name, args);
//        return;
//    }

    if (cmd->value == "defun") {
        handleDefun(tree);
        return;
    }

    // else call

    handleValue(tree);

    throw std::logic_error("Not implemented yet\n");

//    AllocaInst* ptr_ptr1 = Builder.CreateAlloca(PointerTy_struct_list, 0, "ptr");
//    AllocaInst* ptr_ptr2 = Builder.CreateAlloca(PointerTy_tmp1, 0, "ptr2");
//
//
//    Value *RetVal = ConstantFP::get(C, APFloat(7.0));
//
//    Type* IntPtrTy = IntegerType::getInt32Ty(C);
//    Type* Int8Ty = IntegerType::getDoubleTy(C);
//    Constant* allocsize = ConstantExpr::getSizeOf(Int8Ty);
//    allocsize = ConstantExpr::getTruncOrBitCast(allocsize, IntPtrTy);
//
//    Instruction* ptr_arr = CallInst::CreateMalloc(BB, IntPtrTy, Int8Ty, allocsize);
//
//    Value *t1 = Builder.Insert(ptr_arr);
//
//    Value *mydata = Builder.CreateAlloca(Type::getDoublePtrTy(C));
//
//    Builder.CreateStore(t1, mydata);
//
//
//
//    // Finish off the function.
//    Builder.CreateRet(RetVal);
//
//    // Validate the generated code, checking for consistency.
//    verifyFunction(*repl);


}

#endif // HANDLE_AST

