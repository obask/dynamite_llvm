#ifndef EXTERN_LIB
#define EXTERN_LIB

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


struct Object {
    int kind;
};

extern "C"
struct Object* newString(char* ss) {
    return (Object*)ss;
}


typedef std::vector<std::string> KindData;

static std::map<std::string, KindData> caseClasses;


extern "C"
void TypeDef(char* name, char* rawFields, char* rawParents) {
    using namespace std;
    cout << "TypeDef!!!!!!!!!!!!!!!!!!!!!" << endl;
    vector<string> fields = stringSplit(rawFields);
    vector<string> parents = stringSplit(rawParents);

    caseClasses[name] = fields;

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
}


extern "C"
struct Object* callStringPrint(Object* ss) {
    printf("%s\n", (char*)ss);
    return NULL;
}


void initExternalLib(llvm::Module *mod) {
    using namespace llvm;
    LLVMContext &C = getGlobalContext();

    Type *StructTy_Object = mod->getTypeByName("struct.Object");
    PointerType *PointerTy_struct_Object = PointerType::getUnqual(StructTy_Object);

    PointerType *PointerTy_str = PointerType::getUnqual(IntegerType::get(mod->getContext(), 8));
    FunctionType *FT_str = FunctionType::get(PointerTy_struct_Object, {PointerTy_str}, false);
    Function::Create(FT_str, Function::ExternalLinkage, "newString", mod);

    FunctionType *FT1 = FunctionType::get(PointerTy_struct_Object, {PointerTy_struct_Object}, false);
    Function::Create(FT1, Function::ExternalLinkage, "callStringPrint", mod);

    FunctionType *FT_void_str3 = FunctionType::get(llvm::Type::getVoidTy(C), {PointerTy_str, PointerTy_str, PointerTy_str}, false);
    Function::Create(FT_void_str3, Function::ExternalLinkage, "TypeDef", mod);

    FunctionType *FT_void = FunctionType::get(llvm::Type::getVoidTy(C), {}, false);
    Function::Create(FT_void, Function::ExternalLinkage, "debugTypes", mod);

}


#endif
