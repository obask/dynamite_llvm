#ifndef LTOKEN_HPP
#define LTOKEN_HPP

#include <list>
#include "utilz.hpp"

#include "llvm/Support/Casting.h"

using namespace llvm;

namespace Token {

    struct Token {
        enum TokenKind {
            TK_LPAREN,
            TK_RPAREN,
            TK_INT,
            TK_DOUBLE,
            TK_SYMBOL

        };

        const TokenKind SubclassID;

        TokenKind getSubclassID() const {
            return SubclassID;
        }

        Token(TokenKind K) : SubclassID(K) {
        }

        virtual string toString() = 0;
    };

    typedef Token* TokenP;


    struct LParen : public Token {
        LParen() : Token(TK_LPAREN) {
        }

        static bool classof(const Token *S) {
            return S->getSubclassID() == TK_LPAREN;
        }

        virtual string toString() override {
            return "[";
        }
    };


    class RParen : public Token {
    public:

        RParen() : Token(TK_RPAREN) {}

        static bool classof(const Token *S) {
            return S->getSubclassID() == TK_RPAREN;
        }

        virtual string toString() override {
            return "]";
        }
    };


    class IntLit : public Token {
        long value;

    public:
        IntLit(long a) : Token(TK_INT), value(a) {
        };

        static bool classof(const Token *S) {
            return S->getSubclassID() == TK_INT;
        }

        long getValue() {
            return value;
        }

        virtual string toString() override {
            return to_string(value);
        }
    };


    class DoubleLit : public Token {
        double value;

    public:
        DoubleLit(double a) : Token(TK_DOUBLE), value(a) {}

        static bool classof(const Token *S) {
            return S->getSubclassID() == TK_DOUBLE;
        }

        double getValue() {
            return value;
        }

        virtual string toString() override {
            return to_string(value);
        }
    };

    class Symbol : public Token {
        string value;

    public:
        Symbol(string a) : Token(TK_SYMBOL), value(a) {
        };

        string getValue() {
            return value;
        }

        static bool classof(const Token *S) {
            return S->getSubclassID() == TK_SYMBOL;
        }

        virtual string toString() override {
            return value;
        }
    };


    TokenP make_ltoken(string x) {
        if (x == "(") return make<LParen>();
        if (x == ")") return make<RParen>();
        if (is_number(x)) {
            long t1 = stol(x);
            double t2 = stod(x);
            if (t1 == t2) {
                return make<IntLit>(t1);
            } else {
                return make<DoubleLit>(t2);
            }
        } else {
            return make<Symbol>(x);
        }
    }


    list<TokenP> tokenize(string text) {
        list<TokenP> res;
        string curr;
        for (char x: text) {
            switch (x) {
                case '(':
                case ')':
                case ' ':
                case '\t':
                case '\n':
                    if (not curr.empty()) {
                        res.push_back(make_ltoken(curr));
                        curr.clear();
                    }
                    if (x == '(' || x == ')') {
                        res.push_back(make_ltoken(string() + x));
                    }
                    break;
                default:
                    curr += x;
                    break;
            }
        }
        return res;
    }


    string to_string(TokenP ptr) {
        return ptr->toString();
    }


    string to_string1(TokenP ptr) {
        if (isa<LParen>(ptr)) return "[[[";
        if (isa<RParen>(ptr)) return "[[[";
        if (isa<Symbol>(ptr)) {
            Symbol* x = dyn_cast<Symbol>(ptr);
            return string() + "|" + x->getValue() + "|";
        }
        return ptr->toString();
    }

}

#endif

