#ifndef EXTERN_LIB
#define EXTERN_LIB


#include "globals.cpp"

#include "lib_string.cpp"

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


std::vector<std::string> stringSplit(char *str) {
    std::vector<std::string> res;
    char *pch = strtok (str," ");
    while (pch != NULL)
    {
        res.push_back(pch);
        pch = strtok (NULL, " ");
    }
    return res;
}

std::string stringJoin(llvm::SmallVectorImpl<std::string> &args) {
    std::string res;
    for (auto &ss: args) {
        res += ss + " ";
    }
    return res;
}


extern "C"
void TypeDef(char* name, char* rawFields, char* rawParents) {
    using namespace std;
    cout << "TypeDef!!!!!!!!!!!!!!!!!!!!!" << endl;
    vector<string> fields = stringSplit(rawFields);
    vector<string> parents = stringSplit(rawParents);

    caseClasses[name] = fields;

}

extern "C"
LLVMTypeRef* createArrayTypeRef(unsigned len) {
    LLVMTypeRef* xx = new LLVMTypeRef[len];
    return xx;
}

extern "C"
LLVMBool LLVMFalse() {
    return 0;
}

extern "C"
LLVMBool LLVMTrue() {
    return 1;
}

extern "C"
void printlnS(const char* ss) {
    printf("%s", ss);
}

extern "C"
unsigned notNullValueRef(LLVMValueRef ref) {
    return ref == 0;
}

extern "C"
void printlnI(unsigned i) {
    printf("%d", i);
}

std::string
resolveClassName(Object* obj) {
    if (obj->kind == STRING_KIND) {
        return "StringX";
    } else {
        assert(!"something wrong: Object* callFFI1");
    }
}


void*
resolveMetod(Object *obj, std::string methodName) {
    string className = resolveClassName(obj);

    if (!ClassMethods[className].count(methodName)) {
        assert(!"something wrong: methodName not found ");
    }

    Function* fun = ClassMethods[className].at(methodName);

    void *res = TheExecutionEngine->getPointerToFunction(fun);
    return res;
}

extern "C"
long intToLong(int a) {
    return a;
}


extern "C"
Object* callFFI0(char* methodName, Object* obj) {
    cout << "callFFI0! " << methodName << endl;

    void *funPtr = resolveMetod(obj, methodName);

    Func0 tmpFun = (Func0)funPtr;
    Object *res = tmpFun(obj);

    return res;
}

extern "C"
Object* callFFI1(char* methodName, Object* obj, Object* arg1) {
    cout << "callFFI1! " << methodName << endl;

    void *funPtr = resolveMetod(obj, methodName);

    Func1 tmpFun = (Func1)funPtr;
    Object *res = tmpFun(obj, arg1);

    return res;
}


extern "C"
Object* callFFI2(char* methodName, Object* obj, Object* arg1, Object* arg2) {
    cout << "callFFI2! " << methodName << endl;

    void *funPtr = resolveMetod(obj, methodName);

    Func2 tmpFun = (Func2)funPtr;
    Object *res = tmpFun(obj, arg1, arg2);

    return res;
}


extern "C"
void debugTypes() {
    using namespace std;
    printf("--------\n");
    printf("debugTypes ->\n");
    for (auto it : caseClasses) {
        printf("%s:\n", it.first.c_str());
        for (auto field : it.second) {
            printf("    %s\n", field.c_str());
        }
    }
    printf("--------\n");
    for (auto it : ClassMethods) {
        printf("%s:\n", it.first.c_str());
        for (auto field : it.second) {
            printf("    %s\n", field.first.c_str());
        }
    }
    printf("--------\n");
}


void initExternalLib(llvm::Module *mod) {
    using namespace llvm;
    LLVMContext &C = getGlobalContext();

    Type *StructTy_Object = mod->getTypeByName("struct.Object");
    PointerType *PointerTy_Object = PointerType::getUnqual(StructTy_Object);

    PointerType *PointerTy_str = PointerType::getUnqual(IntegerType::get(mod->getContext(), 8));
    FunctionType *FT_str = FunctionType::get(PointerTy_Object, {PointerTy_str}, false);
    Function::Create(FT_str, Function::ExternalLinkage, "newString", mod);

    FunctionType *FT1 = FunctionType::get(PointerTy_Object, {PointerTy_Object}, false);
    Function::Create(FT1, Function::ExternalLinkage, "callStringPrint", mod);

    FunctionType *FT_void_str3 = FunctionType::get(llvm::Type::getVoidTy(C), {PointerTy_str, PointerTy_str, PointerTy_str}, false);
    Function::Create(FT_void_str3, Function::ExternalLinkage, "TypeDef", mod);

    FunctionType *FT_void = FunctionType::get(llvm::Type::getVoidTy(C), {}, false);
    Function::Create(FT_void, Function::ExternalLinkage, "debugTypes", mod);

    FunctionType *FT_object_str = FunctionType::get(PointerTy_Object, {PointerTy_str, PointerTy_Object}, false);
    Function::Create(FT_object_str, Function::ExternalLinkage, "callFFI0", mod);

    FunctionType *FT_object_str_object2 = FunctionType::get(PointerTy_Object, {PointerTy_str, PointerTy_Object, PointerTy_Object}, false);
    Function::Create(FT_object_str_object2, Function::ExternalLinkage, "callFFI1", mod);

    FunctionType *FT_object_str_object3 = FunctionType::get(PointerTy_Object, {PointerTy_str, PointerTy_Object, PointerTy_Object, PointerTy_Object}, false);
    Function::Create(FT_object_str_object3, Function::ExternalLinkage, "callFFI2", mod);
}


#endif
