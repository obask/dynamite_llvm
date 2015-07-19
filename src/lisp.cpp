#include "globals.cpp"

#include "default_types.cpp"
#include "extern_lib.cpp"

#include <iostream>
#include <unordered_set>
#include <fstream>

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

#include "llvm/Support/DynamicLibrary.h"

#include "ltoken.hpp"
#include "SyntaxTree.hpp"
#include "utilz.hpp"

#include "handleAST.cpp"


#include <sstream>


using namespace std;

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




void startLLVM() {
    LLVMContext &C = getGlobalContext();

    FunctionType *FT = FunctionType::get(Type::getVoidTy(C), {}, false);
    Function *theFunction = Function::Create(FT, Function::ExternalLinkage, "INITIALIZE", TheModule);

    BasicBlock *BB = BasicBlock::Create(C, "entry", theFunction);
    Builder.SetInsertPoint(BB);

}


void finishLLVM() {

    Function *initFunction = TheModule->getFunction("INITIALIZE");
    BasicBlock *BB = &initFunction->getEntryBlock();
    Builder.SetInsertPoint(BB);
    // Finish off the function.
    Builder.CreateRetVoid();
    // Validate the generated code, checking for consistency.
    verifyFunction(*initFunction);

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

    void *FPtr2 = TheExecutionEngine->getPointerToFunction(initFunction);
    void (*initFP2)() = (void (*)())(intptr_t)FPtr2;
    initFP2();

    debugTypes();

    // Cast it to the right type (takes no arguments, returns a double) so we
    // can call it as a native function.
    void* (*FP)() = (void* (*)())(intptr_t)FPtr;
    fprintf(stderr, "Evaluated to %ld\n", (long)FP());
}

// TODO correct close local blocks, don't override variable


int plusX(int i) {
    return i + 1;
}


int main() {

    string program;

    ifstream myfile("/Users/oleg/ClionProjects/dynamite_llvm/program.ir", ios::binary);


    std::string line;
    while (std::getline(myfile, line)) {
        program += line + "\n";
    }
    myfile.close();

    cout << "Program:" << endl;
    cout << program << endl;

    list<Token::TokenP> tokens = Token::tokenize(program);

//    cout << "tokens:" << endl;
//    cout << to_string(tokens) << endl;

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

    startLLVM();

    initializeObject(TheModule);

    initExternalLib(TheModule);

    TheModule->dump();

    for (auto cmd: ast) {
        handleIR(cmd);
//        auto res0 = createIR(cmd);
//        cout << res0->toString() << endl;
    }


    auto ff = &LLVMArrayType;

    //    LLVMArrayType(LLVMInt32Type(), 10);
//    sys::DynamicLibrary::AddSymbol("LLVMArrayType", (void*)&LLVMArrayType);

    cout << "finish..." << endl;
    finishLLVM();


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