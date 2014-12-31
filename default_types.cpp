#ifndef DEFAULT_TYPES
#define DEFAULT_TYPES

#include <vector>

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

#endif // DEFAULT_TYPES
