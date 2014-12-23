#ifndef IR_HPP
#define IR_HPP

#include "ltoken.hpp"
#include "utilz.hpp"

#include "llvm/Support/Casting.h"

using namespace std;
using namespace llvm;

namespace IR {

    class IRNode {
    public:

        enum IRNodeKind {
            IR_Define,
            IR_Object,
            IR_Call,
            IR_Symbol,
            IR_Number
        };

    private:

        const IRNodeKind SubclassID;

    public:

        IRNodeKind getSubclassID() const {
            return SubclassID;
        }

        IRNode(IRNodeKind K) : SubclassID(K) {}

        virtual string toString() = 0;

        virtual ~IRNode() {}
    };

    typedef shared_ptr<SyntaxTree> SyntaxTreeP;


    class ABranch : public SyntaxTree {
    private:
        vector<SyntaxTreeP> value;

    public:

        ABranch(vector<SyntaxTreeP> _value)
                : SyntaxTree(NK_Branch), value(_value) {
        }

        static bool classof(const SyntaxTreeP S) {
            return S->getSubclassID() == NK_Branch;
        }

        const vector<SyntaxTreeP> &getValue() {
            return value;
        }

        virtual string toString() override {
            string res = "[ ";
            for (SyntaxTreeP x: value) {
                res += x->toString() + ", ";
            }
            res += "]";
            return res;
        }

    };

    class ALeaf : public SyntaxTree {
    public:

        ALeaf(IRNodeKind K) : SyntaxTree(K) {
        };

        static bool classof(const SyntaxTreeP S) {
            return S->getSubclassID() >= NK_Leaf &&
                    S->getSubclassID() <= NK_Symbol;
        }

        virtual string toString() = 0;
    };

    typedef shared_ptr<ALeaf> ALeafP;

    struct ASymbol : ALeaf {
        string value;

        ASymbol(string _value) : ALeaf(NK_Symbol), value(_value) {
        }

        virtual string toString() override {
            return "\"" + this->value + "\"";
        }

    };

    struct ANumber : ALeaf {
        long value;

        ANumber(long _value)
                : ALeaf(NK_Number), value(_value) {
        }

        virtual string toString() override {
            return to_string(value);
        }

    };

    struct ADouble : ALeaf {
        double value;

        ADouble(double _value)
                : ALeaf(NK_Double), value(_value) {
        }

        virtual string toString() override {
            return to_string(value);
        }

    };


    string to_string(SyntaxTreeP x) {
        return x->toString();
    }

    string to_string(ALeafP x) {
        return x->toString();
    }

    vector<SyntaxTreeP> makeFullAST(list<Token::TokenP> tokens) throw(std::exception) {
        SyntaxTreeP tmp = makeAST(tokens, make_shared<vector<SyntaxTreeP> >());
        shared_ptr<ABranch> tmp1 = dynamic_pointer_cast<ABranch>(tmp);
        return tmp1->getValue();
    }

}

#endif