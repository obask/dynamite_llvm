#ifndef AST_CC
#define AST_CC

#include <iostream>

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
namespace {
/// ExprAST - Base class for all expression nodes.
    class ExprAST {
    public:
        virtual std::string toString() = 0;
        virtual ~ExprAST() {}
    };

/// NumberExprAST - Expression class for numeric literals like "1.0".
    class NumberExprAST : public ExprAST {
        double val;

    public:
        NumberExprAST(double _val) : val(_val) {}

        virtual std::string toString() override {
            return std::to_string(val);
        }
    };

/// VariableExprAST - Expression class for referencing a variable, like "a".
    class VariableExprAST : public ExprAST {
        std::string Name;
    public:
        virtual std::string toString() override {
            return std::string() + "(var " + Name + ")";
        }
        VariableExprAST(const std::string &name) : Name(name) {}
    };

/// BinaryExprAST - Expression class for a binary operator.
    class BinaryExprAST : public ExprAST {
    public:
        BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs) {}
    };

/// CallExprAST - Expression class for function calls.
    class CallExprAST : public ExprAST {
        std::string Callee;
        std::vector<ExprAST*> Args;
    public:

        virtual std::string toString() override {
            std::string res = std::string() + "(call " + Callee;
            for (auto x: Args) {
                res += " " + x->toString();
            }
            return res + ")";
        }

        CallExprAST(const std::string &callee, const std::vector<ExprAST*> &args)
                : Callee(callee), Args(args) {}
    };

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
    class PrototypeAST {
        std::string Name;
        std::vector<std::string> Args;
    public:
        PrototypeAST(const std::string &name, const std::vector<std::string> &args)
                : Name(name), Args(args) {}

        virtual std::string toString() {
            std::string res = std::string() + "(proto " + Name;
            for (auto x: Args) {
                res += " " + x;
            }
            return res + ")";
        }

    };

/// FunctionAST - This class represents a function definition itself.
    class FunctionAST : public ExprAST {
        PrototypeAST *proto;
        ExprAST *body;
    public:
        virtual std::string toString() {
            std::string res = std::string() + "(def " + proto->toString() + body->toString() + ")";
            return res;
        }

        FunctionAST(PrototypeAST *proto_, ExprAST *body_)
        : proto(proto_), body(body_)
        {}
    };
} // end anonymous namespace

#endif // AST_CC
