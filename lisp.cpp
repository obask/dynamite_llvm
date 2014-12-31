
#include "default_types.cpp"

#include <iostream>
#include <vector>
#include <unordered_map>
#include <list>
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
#include "llvm/PassManager.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

#include "ltoken.hpp"
#include "SyntaxTree.hpp"
#include "utilz.hpp"

using namespace std;



typedef unordered_map<string, ALeafP> StateX;


unordered_set<string> MACROS_vector = {"if", "define", "lambda", "defmacro", "quote", "unquote"};


shared_ptr<ADouble> sum_fun(vector<ALeafP> &args) {
    double res = 0.0;
    cout << to_string(args) << endl;
    for (auto x: args) {
        // match x
        if (typeid(*x) == typeid(ADouble)) {
            auto num = dynamic_pointer_cast<ADouble>(x);
            res += num->value;
            continue;
        }
        if (typeid(*x) == typeid(ANumber)) {
            auto num = dynamic_pointer_cast<ANumber>(x);
            res += num->value;
            continue;
        }

        // else {
        throw bad_typeid();
    }
    return make_shared<ADouble>(res);
}

template< typename T1 >
inline bool operator==(const SyntaxTreeP& lhs, const shared_ptr<T1>& rhs) {
    if (typeid(*lhs) == typeid(T1)) {
        auto xs = dynamic_pointer_cast<T1>(lhs);
        return xs->value == rhs->value;
    }
    throw bad_typeid();
}


ALeafP applyAST(SyntaxTreeP &fun, vector<ALeafP> &args, StateX &st) throw (std::exception) {
    cout << "applyAST: " << fun->toString() << endl;
    if (fun == make_shared<ASymbol>("+")) {
        return sum_fun(args);
    }
    if (typeid(*fun) == typeid(ASymbol)) {
        shared_ptr<ASymbol> var_name = dynamic_pointer_cast<ASymbol>(fun);
        if (st.count(var_name->value)) {
            return st.at(var_name->value);
        }
    }

//    case ALambda(vars, code) => evalLambda[ALeaf](vars, args, code, st)

    // else
    throw bad_typeid();
}

ALeafP evalAST(SyntaxTreeP tree, StateX &st) throw (std::exception);

//ALeafP evalLambda(ALambda fun, vector<ALeaf> vals, StateX &st) {
//    cout << "evalLambda" << endl;
//    auto st1 = st;
//    if (fun.args.size() != vals.size())
//        throw logic_error("evalLambda vals number");
//    for (int i=0; i< vals.size(); ++i) {
//        st1[fun.args[i]] = vals[i];
//    }
//    ALeaf res;
//    for (auto cc: fun.code) {
//        res = evalAST(cc, st1);
//    }
//    return res;
//}


ALeafP macroAST(SyntaxTreeP fun, vector<SyntaxTreeP> args, StateX &st) throw (std::exception) {
    if (fun == make_shared<ASymbol>("if")) {
        auto tt = evalAST(args.front(), st);
        if (tt == make_shared<ANumber>(0)) {
            return evalAST(args.at(2), st);
        } else {
            return evalAST(args.at(1), st);
        }
    }
    if (fun == make_shared<ASymbol>("define")) {
        auto xx = dynamic_pointer_cast<ASymbol>(args.at(0));
        st[xx->value] = evalAST(args.at(1), st);
        return make_shared<ANumber>(0);
    }
//    if (fun == make<ASymbol>("lambda")) {
//        auto vars_raw = dynamic_pointer_cast<ABranch>(args.at(0));
//        vector<string> vars;
//        for (const auto &var_raw: vars_raw->getValue()) {
//            auto var = dynamic_pointer_cast<ASymbol>(var_raw);
//            vars.push_back(var->value);
//        }
//        vector<SyntaxTreeP> code = get_tail(args);
//        return make<ALambda>(vars, code);
//    }

//    if (typeid(*fun) == typeid(ALambda)) {
//        cout << "ALambda" << endl;
//        auto lambda_fun = dynamic_pointer_cast<ALambda>(fun);
////        return evalLambda(*lambda_fun, args, st);
//        return make<ANumber>(0);
//    }

//    throw bad_typeid();

      int arr[1];

      cout << arr[1231241251] << endl;

      throw logic_error("bad_typeid macroAST");
}


ALeafP evalAST(SyntaxTreeP tree, StateX &st) throw (std::exception) {
    cout << "evalAST: " << endl;
    // tree match:
    if (typeid(*tree) == typeid(ABranch)) {
        cout << "ABranch" << endl;
        auto xs = dynamic_pointer_cast<ABranch>(tree);
        auto head = xs->getValue().front();
        // head match
        if (typeid(*head) == typeid(ASymbol)) {
            auto cmd = dynamic_pointer_cast<ASymbol>(head);
            vector<SyntaxTreeP> args = get_tail(xs->getValue());
            auto xx = cmd->value;
            if (MACROS_vector.count(xx)) {
                return macroAST(head, args, st);
            } else {
                vector<ALeafP> calc_args;
                for (auto arg: args) {
                    calc_args.push_back(evalAST(arg, st));
                }
                return applyAST(head, calc_args, st);
            }
        }
        if (typeid(*head) == typeid(ABranch)) {
            // TODO add ABranch lambda support
            throw logic_error("this feature not supported yet");
        }
        // else
        throw bad_typeid();
    }
    if (typeid(*tree) == typeid(ASymbol)) {
        auto ss = dynamic_pointer_cast<ASymbol>(tree);
        if (st.count(ss->value)) {
            return st[ss->value];
        } else {
            return ss;
        }
    }
    if (typeid(*tree) == typeid(ANumber)) {
        ALeafP res = dynamic_pointer_cast<ANumber>(tree);
        return res;
    }
    if (typeid(*tree) == typeid(ADouble)) {
        ALeafP res = dynamic_pointer_cast<ADouble>(tree);
        return res;
    }
    // else
    throw bad_typeid();
}


void print_state(const StateX& st) {
    cout << "state:" << endl;
    for (auto x: st) {
        cout << x.first << ":" << x.second->toString() << endl;
    }
    cout << "--" << endl;
}

using namespace llvm;

static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, AllocaInst *> NamedValues;
static FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;



unordered_map<string, unordered_map<string, int> > DeclState;

// Special case '=' because we don't want to emit the LHS as an expression.
//if (Op == '=') {
//// Assignment requires the LHS to be an identifier.
//VariableExprAST *LHSE = dynamic_cast<VariableExprAST *>(LHS);
//if (!LHSE)
//return ErrorV("destination of '=' must be a variable");
//// Codegen the RHS.
//Value *Val = RHS->Codegen();
//if (Val == 0)
//return 0;
//
//// Look up the name.
//Value *Variable = NamedValues[LHSE->getName()];
//if (Variable == 0)
//return ErrorV("Unknown variable name");
//
//Builder.CreateStore(Val, Variable);
//return Val;
//}


Value *handleCall(string Op, vector<SyntaxTreeP> args);

Value *internalCall(string Op, vector<Value*> calc_args);

void handleIR(SyntaxTreeP tree);

Value *handleValue(SyntaxTreeP tt);


Value *handleIfExpr(SyntaxTreeP cond, SyntaxTreeP thenBranch, SyntaxTreeP elseBranch) {
    LLVMContext &C = getGlobalContext();

    cout << "handleIfExpr: " << cond->toString() << endl;

    Value *condVal0 = handleValue(cond);
    if (condVal0 == 0)
        return 0;

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



Value *handleValue(SyntaxTreeP tt) {
    LLVMContext &C = getGlobalContext();
    cout << "handleValue: " << tt->toString() << endl;

    if (typeid(*tt) == typeid(ASymbol)
            && tt->getString() == "null") {

        cout << "dbg null 1 " << endl;

        PointerType* PointerTy_struct_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));
        Value* res = ConstantPointerNull::get(PointerTy_struct_Object);

        cout << "dbg null 2 " << endl;

        return res;
    }

    if (typeid(*tt) == typeid(ASymbol)) {
        string var = tt->getString();
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
        TheModule->dump();
        return internalCall("createDouble", args);
    }

    if (typeid(*tt) == typeid(ABranch)
        && typeid(*tt->elemAt(0)) == typeid(ASymbol)
        && tt->elemAt(0)->getString() == "if") {
        return handleIfExpr(tt->elemAt(1), tt->elemAt(2), tt->elemAt(3));
    }

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


void handleConstructor(string structName0, const vector<string> &fields) {
    LLVMContext &C = getGlobalContext();
    cout << "handleConstructor: " << structName0 << endl;
    Module *mod = TheModule;

    string structName = "struct." + structName0;

    StructType* StructTy_struct_Object = mod->getTypeByName("struct.Object");
    PointerType* PointerTy_struct_Object = PointerType::get(StructTy_struct_Object, 0);

    // Type Definitions
    StructType *StructTy_struct_Current = mod->getTypeByName(structName);

    PointerType* PointerTy_struct_Current = PointerType::get(StructTy_struct_Current, 0);

    PointerType* PointerTy_2 = PointerType::get(Type::getDoubleTy(C), 0);

    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(C, 8), 0);

    std::vector<Type*>FuncTy_6_args;
    FuncTy_6_args.push_back(IntegerType::get(C, 64));
    FunctionType* FuncTy_6 = FunctionType::get(
            /*Result=*/PointerTy_4,
            /*Params=*/FuncTy_6_args,
            /*isVarArg=*/false);

    PointerType* PointerTy_5 = PointerType::get(FuncTy_6, 0);

    PointerType* PointerTy_7 = PointerType::get(StructTy_struct_Object, 0);

    PointerType* PointerTy_8 = PointerType::get(IntegerType::get(C, 32), 0);

    std::vector<Type*>FuncTy_9_args;
    FunctionType* FuncTy_9 = FunctionType::get(
            /*Result=*/IntegerType::get(mod->getContext(), 32),
            /*Params=*/FuncTy_9_args,
            /*isVarArg=*/false);


    cout << "dbg 2" << endl;

    std::vector<Type*>FuncTy_1_args;
    for (auto &arg : fields) {
        FuncTy_1_args.push_back(PointerTy_struct_Object);
    }
    FunctionType* FuncTy_1 = FunctionType::get(
            /*Result=*/PointerTy_struct_Object,
            /*Params=*/FuncTy_1_args,
            /*isVarArg=*/false);

    // Function Declarations

    Function* func_createCurrent = mod->getFunction("create" + structName0);
    if (!func_createCurrent) {
        func_createCurrent = Function::Create(
                /*Type=*/ FuncTy_1,
                /*Linkage=*/ GlobalValue::ExternalLinkage,
                /*Name=*/ "create" + structName0, mod);
    }

    Function* func_malloc = mod->getFunction("malloc");
    if (!func_malloc) {
        func_malloc = Function::Create(
                /*Type=*/FuncTy_6,
                /*Linkage=*/GlobalValue::ExternalLinkage,
                /*Name=*/"malloc", mod); // (external, no body)
        func_malloc->setCallingConv(CallingConv::C);
    }

    // Constant Definitions
    ConstantInt* KindDouble = ConstantInt::get(C, APInt(32, 7));

    // Function Definitions

    cout << "dbg 3" << endl;

    // Function: createDouble (func_createDouble)

    // If F took a different number of args, reject.
    if (func_createCurrent->arg_size() != fields.size()) {
        throw std::logic_error("redefinition of function with different # args");
    }

    PointerType* ptr_ptr_custom_ty = PointerType::getUnqual(PointerTy_struct_Current);


    BasicBlock* label_entry = BasicBlock::Create(C, "entry", func_createCurrent,0);

    Function::arg_iterator args_it = func_createCurrent->arg_begin();

    vector<LoadInst*> input_args;
    for (auto &field : fields) {

        args_it->setName(field);

        // Block entry (label_entry)
        AllocaInst* ptr_xx_addr = new AllocaInst(PointerTy_struct_Object, "addr_of_" + field, label_entry);
        ptr_xx_addr->setAlignment(8);

        StoreInst* void_15 = new StoreInst(args_it, ptr_xx_addr, false, label_entry);
        void_15->setAlignment(8);

        LoadInst* res_arg = new LoadInst(ptr_xx_addr, field + "_on_stack", false, label_entry);

        input_args.push_back(res_arg);

        args_it++;
    }


    // fixme bad size of allocation
    CallInst* ptr_call = CallInst::Create(func_malloc, ConstantExpr::getSizeOf(StructTy_struct_Current), "call", label_entry);

    cout << "dbg 4" << endl;

    CastInst* ptr_malloc = new BitCastInst(ptr_call, PointerTy_struct_Current, "ptr_malloc", label_entry);

    cout << "dbg 4.0" << endl;

    AllocaInst* ptr_curr = new AllocaInst(PointerTy_struct_Current, "ptr", label_entry);
    ptr_curr->setAlignment(8);

    cout << "dbg 4.1" << endl;

    StoreInst* void_17 = new StoreInst(ptr_malloc, ptr_curr, false, label_entry);
    void_17->setAlignment(8);

    cout << "dbg 4.2" << endl;


    LoadInst* curr = new LoadInst(ptr_curr, "curr", false, label_entry);
    curr->setAlignment(8);

    cout << "dbg 4.3" << endl;

    std::vector<Value*> ptr_parent_indices;
    ptr_parent_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    ptr_parent_indices.push_back(ConstantInt::get(C, APInt(32, 0)));

    cout << "dbg 5" << endl;

    Instruction* ptr_parent = GetElementPtrInst::Create(curr, ptr_parent_indices, "parent", label_entry);

    cout << "dbg 5.1" << endl;

    std::vector<Value*> ptr_kind_indices;
    ptr_kind_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    ptr_kind_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    Instruction* ptr_kind = GetElementPtrInst::Create(ptr_parent, ptr_kind_indices, "kind", label_entry);

    cout << "dbg 5.2" << endl;


    StoreInst* void_19 = new StoreInst(KindDouble, ptr_kind, false, label_entry);
    void_19->setAlignment(4);



//    curr
    cout << "dbg 6" << endl;

    for (int i=0; i < fields.size(); ++i) {
        cout << "dbg 6.1" << fields[i] << endl;

        std::vector<Value*> ptr_val_indices;
        ptr_val_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
        ptr_val_indices.push_back(ConstantInt::get(C, APInt(32, 1)));

        Instruction* ptr_val = GetElementPtrInst::Create(curr, ptr_val_indices, "ptr__" + fields[i], label_entry);

        cout << "dbg 6.2" << fields[i] << endl;

        StoreInst* void_22 = new StoreInst(input_args[i], ptr_val, false, label_entry);
        void_22->setAlignment(8);

    }

    // RET

    cout << "dbg 7" << endl;

//    LoadInst* ptr_23 = new LoadInst(curr, "", false, label_entry);
//    ptr_23->setAlignment(8);


    curr->dump();
    curr->getType()->dump();

    CastInst* ptr_res = new BitCastInst(curr, PointerTy_struct_Object, "", label_entry);

    ReturnInst::Create(C, ptr_res, label_entry);

}


Value *handleStruct(string structName0, const vector<string> &fields) {
    LLVMContext &C = getGlobalContext();
    cout << "handleStruct: " << structName0 << endl;

    string structName = "struct." + structName0;
    Module *mod = TheModule;

    StructType* StructTy_struct_Object = mod->getTypeByName("struct.Object");
    PointerType* PointerTy_struct_Object = PointerType::get(StructTy_struct_Object, 0);

    // Type Definitions
    StructType *StructTy_struct_Current = mod->getTypeByName(structName);
    if (StructTy_struct_Current) {
        assert("redifinition of struct");
        return NULL;
    } else {
        StructTy_struct_Current = StructType::create(C, structName);
    }
    std::vector<Type*> StructTy_struct_Current_fields;
    StructTy_struct_Current_fields.push_back(StructTy_struct_Object);
    for (auto &field : fields) {
        StructTy_struct_Current_fields.push_back(PointerTy_struct_Object);
    }

    cout << "dbg 1" << endl;

    if (StructTy_struct_Current->isOpaque()) {
        StructTy_struct_Current->setBody(StructTy_struct_Current_fields, /*isPacked=*/false);
    } else {
        throw std::logic_error("if (StructTy_struct_Current->isOpaque()) {");
    }

    ConstantInt* const_int32_13 = ConstantInt::get(C, APInt(32, 0));
    ConstantFP* const_double_14 = ConstantFP::get(C, APFloat(1.200000e+01));

    handleConstructor(structName0, fields);

    return NULL;
}

Value *internalCall(string Op, vector<Value*> calc_args) {
    LLVMContext &C = getGlobalContext();
    cout << "internalCall" << calc_args.size() << " " << Op << endl;

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
    Function *F = TheModule->getFunction(Op);
    assert(F && "binary operator not found!");

//    Value *Ops[] = { L, R };
    return Builder.CreateCall(F, calc_args, "binop");
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




Function *handleDefun(SyntaxTreeP tree) {
    LLVMContext &C = getGlobalContext();
    cout << "handleDefun" << endl;

    PointerType* PointerTy_struct_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));

    string name = tree->elemAt(1)->getString();
    vector<SyntaxTreeP> proto = tree->elemAt(2)->getVector();
    vector<string> args;

    cout << "DBG 2" << endl;

    for (SyntaxTreeP az: proto) {
        args.push_back(az->getString());
    }

    std::vector<Type *> argsTypes(args.size(), PointerTy_struct_Object);
    FunctionType *FT = FunctionType::get(PointerTy_struct_Object, argsTypes, false);

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
        AllocaInst *Alloca = TmpB.CreateAlloca(Type::getInt32Ty(C), 0, varName.c_str());
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


void handleIR(SyntaxTreeP tree) {
    LLVMContext &C = getGlobalContext();
    cout << "handleIR " << tree->toString() << endl;

    SyntaxTreeP tmp2 = tree->elemAt(0);
    shared_ptr<ASymbol> cmd = dynamic_pointer_cast<ASymbol>(tmp2);

    if (cmd->value == "object") {
        string name = tree->elemAt(1)->getString();
        vector<string> args;
        args.push_back(tree->elemAt(2)->getString());
        args.push_back(tree->elemAt(3)->getString());
        handleStruct(name, args);
        return;
    }

    if (cmd->value == "defun") {
        handleDefun(tree);
        return;
    }

    if (cmd->value == "set") {
        return handleSet(tree);
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


void finishLLVM() {
    // Optimize the function.
//  TheFPM->run(*repl);

    fflush(stdout);

    TheModule->dump();

    Function *repl = TheModule->getFunction("main");

    if (!repl) {
        throw std::logic_error("finishLLVM main not found");
    }

    TheExecutionEngine->finalizeObject();
    // JIT the function, returning a function pointer.
    void *FPtr = TheExecutionEngine->getPointerToFunction(repl);
    // Cast it to the right type (takes no arguments, returns a double) so we
    // can call it as a native function.
    void* (*FP)() = (void* (*)())(intptr_t)FPtr;
    fprintf(stderr, "Evaluated to %ld\n", (long)FP());

}

// TODO correct close local blocks, don't override variable

int main() {
    string program;
    program += ""
//        "(defun fact (n) (if (< n 2)                \n"
//        "                    1                      \n"
//        "                    (* n (fact (- n 1))))) \n"
        "(object List field1 field2)          \n"
        "(defun main () (createList 3.0 null) \n"
        "               null)                 \n";

    cout << "program:" << endl;
    cout << program << endl;

    list<Token::TokenP> tokens = Token::tokenize(program);

    cout << "tokens:" << endl;
    cout << to_string(tokens) << endl;


    auto ast = makeFullAST(tokens);

    cout << "AST:" << endl;
    for (auto cmd: ast) {
        cout << cmd->toString() << endl;
    }

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    LLVMContext &Context = getGlobalContext();

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

    cout << "Codegen: " << ast.size() << endl;

    initializeDefaultTypes(TheModule);

    for (auto cmd: ast) {
        handleIR(cmd);
//        auto res0 = createIR(cmd);
//        cout << res0->toString() << endl;
    }

    finishLLVM();

    cout << "finish!" << endl;

    return 0;
}

//
//int main() {
//    vector<int> xs = {1,2,3};
//    auto ys = get_tail(xs);
//    for (auto x: ys) {
//        cout << x << endl;
//    }
//}