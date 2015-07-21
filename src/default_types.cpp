#ifndef DEFAULT_TYPES
#define DEFAULT_TYPES

#include "globals.cpp"

#include <vector>
#include <iostream>
#include <cstring>
#include <unordered_map>

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

#include "extern_lib.cpp"

#include "SyntaxTree.hpp"

using namespace std;

using namespace llvm;


void initializeObject(Module *mod) {
    LLVMContext &C = getGlobalContext();

    StructType *StructTy_struct_Object = mod->getTypeByName("struct.Object");
    if (!StructTy_struct_Object) {
        StructTy_struct_Object = StructType::create(C, "struct.Object");
    }
    std::vector<Type*>StructTy_struct_Object_fields;
    StructTy_struct_Object_fields.push_back(IntegerType::get(mod->getContext(), 32));
    if (StructTy_struct_Object->isOpaque()) {
        StructTy_struct_Object->setBody(StructTy_struct_Object_fields, /*isPacked=*/false);
    }

}


void initializeDefaultTypes(Module *mod) {
    LLVMContext &C = getGlobalContext();

    initializeObject(mod);

    Type* StructTy_struct_Object = mod->getTypeByName("struct.Object");
    PointerType *PointerTy_struct_Object = PointerType::getUnqual(StructTy_struct_Object);

    // Type Definitions
    StructType *StructTy_struct_Double = mod->getTypeByName("struct.Double");
    if (!StructTy_struct_Double) {
        StructTy_struct_Double = StructType::create(C, "struct.Double");
    }
    std::vector<Type*>StructTy_struct_Double_fields;
    StructTy_struct_Double_fields.push_back(StructTy_struct_Object);
    StructTy_struct_Double_fields.push_back(Type::getDoubleTy(mod->getContext()));
    if (StructTy_struct_Double->isOpaque()) {
        StructTy_struct_Double->setBody(StructTy_struct_Double_fields, /*isPacked=*/false);
    }

    PointerType* PointerTy_0 = PointerType::get(StructTy_struct_Double, 0);

    std::vector<Type*>FuncTy_1_args;
    FuncTy_1_args.push_back(Type::getDoubleTy(mod->getContext()));
    FunctionType* FuncTy_1 = FunctionType::get(
            /*Result=*/PointerTy_struct_Object,
            /*Params=*/FuncTy_1_args,
            /*isVarArg=*/false);

    PointerType* PointerTy_2 = PointerType::get(Type::getDoubleTy(mod->getContext()), 0);

    PointerType* PointerTy_3 = PointerType::get(PointerTy_0, 0);

    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);

    std::vector<Type*>FuncTy_6_args;
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 64));
    FunctionType* FuncTy_6 = FunctionType::get(
            /*Result=*/PointerTy_4,
            /*Params=*/FuncTy_6_args,
            /*isVarArg=*/false);

    PointerType* PointerTy_5 = PointerType::get(FuncTy_6, 0);

    PointerType* PointerTy_7 = PointerType::get(StructTy_struct_Object, 0);

    PointerType* PointerTy_8 = PointerType::get(IntegerType::get(mod->getContext(), 32), 0);

    std::vector<Type*>FuncTy_9_args;
    FunctionType* FuncTy_9 = FunctionType::get(
            /*Result=*/IntegerType::get(mod->getContext(), 32),
            /*Params=*/FuncTy_9_args,
            /*isVarArg=*/false);

    PointerType* PointerTy_10 = PointerType::get(FuncTy_1, 0);


    // Function Declarations

    Function* func_createDouble = mod->getFunction("createDouble");
    if (!func_createDouble) {
        func_createDouble = Function::Create(
                /*Type=*/FuncTy_1,
                /*Linkage=*/GlobalValue::ExternalLinkage,
                /*Name=*/"createDouble", mod);
        func_createDouble->setCallingConv(CallingConv::C);
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
    ConstantInt* KindDouble = ConstantInt::get(mod->getContext(), APInt(32, 1));
    ConstantInt* const_int64_12 = ConstantInt::get(mod->getContext(), APInt(64, 16));
    ConstantInt* const_int32_13 = ConstantInt::get(mod->getContext(), APInt(32, 0));
    ConstantFP* const_double_14 = ConstantFP::get(mod->getContext(), APFloat(1.200000e+01));

    // Function Definitions

    // Function: createDouble (func_createDouble)
    {
        Function::arg_iterator args = func_createDouble->arg_begin();
        Value* double_xx = args++;
        double_xx->setName("xx");

        BasicBlock* label_entry = BasicBlock::Create(mod->getContext(), "entry",func_createDouble,0);

        // Block entry (label_entry)
        AllocaInst* ptr_xx_addr = new AllocaInst(Type::getDoubleTy(mod->getContext()), "xx.addr", label_entry);
        ptr_xx_addr->setAlignment(8);
        AllocaInst* ptr_ptr = new AllocaInst(PointerTy_0, "ptr", label_entry);
        ptr_ptr->setAlignment(8);
        StoreInst* void_15 = new StoreInst(double_xx, ptr_xx_addr, false, label_entry);
        void_15->setAlignment(8);
        CallInst* ptr_call = CallInst::Create(func_malloc, const_int64_12, "call", label_entry);
        ptr_call->setCallingConv(CallingConv::C);
        ptr_call->setTailCall(false);
        AttributeSet ptr_call_PAL;
        ptr_call->setAttributes(ptr_call_PAL);

        CastInst* ptr_16 = new BitCastInst(ptr_call, PointerTy_0, "", label_entry);
        StoreInst* void_17 = new StoreInst(ptr_16, ptr_ptr, false, label_entry);
        void_17->setAlignment(8);
        LoadInst* ptr_18 = new LoadInst(ptr_ptr, "", false, label_entry);
        ptr_18->setAlignment(8);
        std::vector<Value*> ptr_parent_indices;
        ptr_parent_indices.push_back(const_int32_13);
        ptr_parent_indices.push_back(const_int32_13);
        Instruction* ptr_parent = GetElementPtrInst::Create(ptr_18, ptr_parent_indices, "parent", label_entry);
        std::vector<Value*> ptr_kind_indices;
        ptr_kind_indices.push_back(const_int32_13);
        ptr_kind_indices.push_back(const_int32_13);
        Instruction* ptr_kind = GetElementPtrInst::Create(ptr_parent, ptr_kind_indices, "kind", label_entry);
        StoreInst* void_19 = new StoreInst(KindDouble, ptr_kind, false, label_entry);
        void_19->setAlignment(4);
        LoadInst* double_20 = new LoadInst(ptr_xx_addr, "", false, label_entry);
        double_20->setAlignment(8);
        LoadInst* ptr_21 = new LoadInst(ptr_ptr, "", false, label_entry);
        ptr_21->setAlignment(8);
        std::vector<Value*> ptr_val_indices;
        ptr_val_indices.push_back(const_int32_13);
        ptr_val_indices.push_back(KindDouble);
        Instruction* ptr_val = GetElementPtrInst::Create(ptr_21, ptr_val_indices, "val", label_entry);
        StoreInst* void_22 = new StoreInst(double_20, ptr_val, false, label_entry);
        void_22->setAlignment(8);
        LoadInst* ptr_23 = new LoadInst(ptr_ptr, "", false, label_entry);
        ptr_23->setAlignment(8);

        CastInst* ptr_res = new BitCastInst(ptr_23, PointerTy_struct_Object, "res", label_entry);

        ReturnInst::Create(mod->getContext(), ptr_res, label_entry);

    }

}



void handleConstructor(std::string structName0, const std::vector<std::string> &fields) {
    LLVMContext &C = getGlobalContext();
    cout << "handleConstructor: " << structName0 << endl;
    for (auto it : fields) {
        cout << it << " ";
    }
    cout << endl;

    Module *mod = TheModule;

    string structName = "struct." + structName0;

    cout << "dbg 0.1" << endl;

    StructType* StructTy_struct_Object = mod->getTypeByName("struct.Object");
    PointerType* PointerTy_struct_Object = PointerType::get(StructTy_struct_Object, 0);

    // Type Definitions
    StructType *StructTy_struct_Current = mod->getTypeByName(structName);

    cout << "dbg 0.2" << endl;

    PointerType* PointerTy_struct_Current = PointerType::get(StructTy_struct_Current, 0);

    cout << "dbg 0.3" << endl;

    PointerType* PointerTy_2 = PointerType::get(Type::getDoubleTy(C), 0);

    cout << "dbg 0.4" << endl;

    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(C, 8), 0);

    cout << "dbg 0.5" << endl;

    std::vector<Type*>FuncTy_6_args;
    FuncTy_6_args.push_back(IntegerType::get(C, 64));
    FunctionType* FuncTy_6 = FunctionType::get(
            /*Result=*/PointerTy_4,
            /*Params=*/FuncTy_6_args,
            /*isVarArg=*/false);

    PointerType* PointerTy_5 = PointerType::get(FuncTy_6, 0);

    PointerType* PointerTy_7 = PointerType::get(StructTy_struct_Object, 0);

    cout << "dbg 0.1" << endl;

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

    cout << "OK handleConstructor " << structName0 << endl;

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


Value *handleString(string varRaw) {
    LLVMContext &C = getGlobalContext();
    Module *mod = TheModule;
    string varStr = varRaw.substr(1, varRaw.length() - 2);
    Function* fun = TheModule->getFunction("newString");

    // Type Definitions
    ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(C, 8), varStr.length() + 1);
    PointerType* PointerTy_1 = PointerType::getUnqual(ArrayTy_0);

    // Constant Definitions
    Constant *const_array_4 = ConstantDataArray::getString(mod->getContext(), varStr, true);

    GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
            /*Type=*/ArrayTy_0,
            /*isConstant=*/true,
            /*Linkage=*/GlobalValue::PrivateLinkage,
            /*Initializer=*/const_array_4, // has initializer, specified below
            /*Name=*/".str");
    gvar_array__str->setAlignment(1);

    std::vector<Constant*> const_ptr_5_indices;
    const_ptr_5_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    const_ptr_5_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    Constant* const_ptr_5 = ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_5_indices);

//    return Builder.CreateCall(fun, const_ptr_5, "newString");
    return const_ptr_5;
}


Constant *internalString(string varRaw) {
    LLVMContext &C = getGlobalContext();
    Module *mod = TheModule;

    // Type Definitions
    ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(C, 8), varRaw.length() + 1);

    // Constant Definitions
    Constant *const_array_4 = ConstantDataArray::getString(C, varRaw, true);

    GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
            /*Type=*/ArrayTy_0,
            /*isConstant=*/true,
            /*Linkage=*/GlobalValue::PrivateLinkage,
            /*Initializer=*/const_array_4, // has initializer, specified below
            /*Name=*/".str");
    gvar_array__str->setAlignment(1);

    std::vector<Constant*> const_ptr_5_indices;
    const_ptr_5_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    const_ptr_5_indices.push_back(ConstantInt::get(C, APInt(32, 0)));
    Constant* const_ptr_5 = ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_5_indices);

    return const_ptr_5;
}


std::string createFFIName(std::string clazz, std::string method) {
    return clazz + "_" + method;
}


void setNewClassMethod(std::string className, std::string methodName, Function* theFunction) {
    cout << "setNewClassMethod " << className << "::" << methodName << endl;

    if (!ClassMethods.count(className)) {
        ClassMethods[className] = unordered_map<std::string, Function*>();
    }

    ClassMethods[className][methodName] = theFunction;

}


Function *handleNewFFI(SyntaxTreeP tree, std::string className) {
    LLVMContext &C = getGlobalContext();
    cout << "handleNewFFI" << endl;

    PointerType* PointerTy_Object = PointerType::getUnqual(TheModule->getTypeByName("struct.Object"));

    string methodName = tree->elemAt(1)->getString();

    assert("???" == tree->elemAt(3)->getString());

    vector<SyntaxTreeP> proto = tree->elemAt(2)->getVector();
    vector<string> args;

    cout << "DBG 2" << endl;

    for (SyntaxTreeP az: proto) {
        args.push_back(az->getString());
    }

    std::vector<Type *> argsTypes(args.size(), PointerTy_Object);
    FunctionType *FT = FunctionType::get(PointerTy_Object, argsTypes, false);

//    FunctionType *FT = FunctionType::get(Type::getInt32Ty(C), {}, false);
    Function *theFunction = Function::Create(FT, Function::ExternalLinkage, createFFIName(className, methodName), TheModule);

    setNewClassMethod(className, methodName, theFunction);

    return theFunction;
//    for (auto xx: body) {
//        RetVal = handleValue(xx);
//    }

}


void handleTypeDef(StringRef rawClassName,
        SmallVectorImpl<string> &fields,
        SmallVectorImpl<string> &parents,
        std::vector<SyntaxTreeP> body) {
    LLVMContext &C = getGlobalContext();
    Module *mod = TheModule;

    Function* theFunction = mod->getFunction("INITIALIZE");
    assert(theFunction && "mod->getFunction(\"INITIALIZE\");");
    BasicBlock *BB = &theFunction->getEntryBlock();
    Builder.SetInsertPoint(BB);

    Constant *className = internalString(rawClassName);
    Constant *classFields = internalString(stringJoin(fields));
    Constant *classParents = internalString(stringJoin(parents));

    Function* fun = TheModule->getFunction("TypeDef");
    Builder.CreateCall3(fun, className, classFields, classParents);

    // костыль для копирования векторов
    vector<string> vec_tmp;
    for (auto xx : fields) {
        vec_tmp.push_back(xx);
    }
//    if (!fields.empty()) {
    handleStruct(rawClassName, vec_tmp);
//    }
    for (auto defMethod : body) {
        SyntaxTreeP functionBody = defMethod->elemAt(3);

        if (typeid(*functionBody) == typeid(ASymbol)
                && functionBody->getString() == "EmptyTree") {
            ;
        } else if (typeid(*functionBody) == typeid(ASymbol)
                && functionBody->getString() == "???") {
            handleNewFFI(defMethod, rawClassName);
        } else {

            vector<SyntaxTreeP> &functionArgs = defMethod->elemAt(2)->getVector();

            std::vector<SyntaxTreeP> functionArgsNew;
            functionArgsNew.push_back(make_shared<ASymbol>("this"));
            for (auto xx : functionArgs) {
                functionArgsNew.push_back(xx);
            }

//            shared_ptr<vector<SyntaxTreeP>> tmp = dynamic_pointer_cast<vector<SyntaxTreeP>>(defMethod);
//            tmp[2] = make_shared<ABranch>(new vector(functionArgsNew));

            functionArgs.assign(functionArgsNew.begin(), functionArgsNew.end());

            Function *ff = handleDefun(defMethod);
        }
    }

}



#endif // DEFAULT_TYPES
