#ifndef PARSER_HPP
#define PARSER_HPP

#include "ltoken.hpp"
#include "utilz.hpp"

#include "llvm/Support/Casting.h"

using namespace std;
using namespace llvm;


class SyntaxTree {
public:

    enum SyntaxTreeKind {
        NK_Branch,
        NK_Leaf,
        NK_Symbol,
        NK_Number,
        NK_Double
    };

private:

    const SyntaxTreeKind SubclassID;

public:

    SyntaxTreeKind getSubclassID() const { return SubclassID; }
    SyntaxTree(SyntaxTreeKind K) : SubclassID(K) {}

    virtual vector<shared_ptr<SyntaxTree> > &getVector() {
        throw std::logic_error("virtual vector<shared_ptr<SyntaxTree> > &getVector()");
    }
    virtual shared_ptr<SyntaxTree> elemAt(int i) { return NULL; }
    virtual string getString() { return ""; }
    virtual string toString() = 0;
    virtual ~SyntaxTree() {}
};

typedef shared_ptr<SyntaxTree> SyntaxTreeP;


class ABranch : public SyntaxTree {
private:
    vector<SyntaxTreeP> value;

public:

    ABranch(vector<SyntaxTreeP> _value)
            : SyntaxTree(NK_Branch), value(_value)
    {}

    static bool classof(const SyntaxTreeP S) {
        return S->getSubclassID() == NK_Branch;
    }

    const vector<SyntaxTreeP> &getValue() {
        return value;
    }

    virtual vector<SyntaxTreeP> &getVector() override {
        return value;
    }

    virtual SyntaxTreeP elemAt(int i) override {
        return value.at(i);
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

    ALeaf(SyntaxTreeKind K) : SyntaxTree(K) {};

    static bool classof(const SyntaxTreeP S) {
        return S->getSubclassID() >= NK_Leaf &&
                S->getSubclassID() <= NK_Symbol;
    }

    virtual string toString() = 0;
};

typedef shared_ptr<ALeaf> ALeafP;

struct ASymbol: ALeaf {
    string value;
    ASymbol(string _value) : ALeaf(NK_Symbol), value(_value)
    {}

    virtual string getString() override {
        return value;
    }

    virtual string toString() override {
        return "\"" + this->value + "\"";
    }

};

struct ANumber: ALeaf {
    long value;
    ANumber(long _value)
            : ALeaf(NK_Number), value(_value)
    {}

    virtual string toString() override {
        return to_string(value);
    }

};

struct ADouble: ALeaf {
    double value;
    ADouble(double _value)
            : ALeaf(NK_Double), value(_value)
    {}

    virtual string toString() override {
        return to_string(value);
    }

};


SyntaxTreeP makeAST(list<Token::TokenP> &tokens, shared_ptr<vector<SyntaxTreeP> > state) throw (std::exception) {
    if (tokens.empty()) {
        shared_ptr<ABranch> tt = make_shared<ABranch>(*state);
        return tt; // dynamic_pointer_cast<SyntaxTree>(tt);
    } else {
        auto head = tokens.front();
        tokens.pop_front();
        if (isa<Token::LParen>(head)) {
            auto st1 = makeAST(tokens, make_shared<vector<SyntaxTreeP> >());
            state->push_back(st1);
            return makeAST(tokens, state);
        }
        if (isa<Token::RParen>(head)) {
            return make_shared<ABranch>(*state);
        }
        if (isa<Token::Symbol>(head)) {
            Token::Symbol* x = dyn_cast<Token::Symbol>(head);
            state->push_back(make_shared<ASymbol>(x->getValue()));
            return makeAST(tokens, state);
        }
        if (isa<Token::IntLit>(head)) {
            Token::IntLit* x = dyn_cast<Token::IntLit>(head);
            state->push_back(make_shared<ANumber>(x->getValue()));
            return makeAST(tokens, state);
        }
        if (isa<Token::DoubleLit>(head)) {
            Token::DoubleLit* x = dyn_cast<Token::DoubleLit>(head);
            state->push_back(make_shared<ADouble>(x->getValue()));
            return makeAST(tokens, state);
        }
        throw 0;
    }
}


string to_string(SyntaxTreeP x) {
    return x->toString();
}

string to_string(ALeafP x) {
    return x->toString();
}

vector<SyntaxTreeP> makeFullAST(list<Token::TokenP> tokens) throw (std::exception) {
    SyntaxTreeP tmp = makeAST(tokens, make_shared<vector<SyntaxTreeP> >());
    shared_ptr<ABranch> tmp1= dynamic_pointer_cast<ABranch>(tmp);
    return tmp1->getValue();
}

#endif