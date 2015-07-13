#ifndef GLOBALS_CPP
#define GLOBALS_CPP


#include <iostream>
#include <vector>
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

#include "ltoken.hpp"
#include "SyntaxTree.hpp"
#include "utilz.hpp"


using namespace std;

typedef unordered_map<string, ALeafP> StateX;


struct Object {
    int kind;
};

const int STRING_KIND = 123;


typedef Object* (*Func0)(Object*);
typedef Object* (*Func1)(Object*, Object*);
typedef Object* (*Func2)(Object*, Object*, Object*);

static std::unordered_map<std::string,
std::unordered_map<std::string, Function* >> ClassMethods;


using namespace llvm;

static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
static std::map<std::string, AllocaInst *> NamedValues;



typedef std::vector<std::string> KindData;

static std::map<std::string, KindData> caseClasses;


static FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;



unordered_map<string, unordered_map<string, int> > DeclState;

const int VEC_SIZE = 7;

Function *handleDefun(SyntaxTreeP tree);


#endif
