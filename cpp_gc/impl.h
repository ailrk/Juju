#include <memory>
#include <stdio.h>
#include <vector>

// IMP language with a tree walk interpreter.
// The langauge is just used for testing different garbage collection
// algorithms.
//
// Support integer, boolean, dynamic allocated memory block called Chunk .
// There is no explicit syntax for declaring chunk. To initalize simply assign
// to cells.

// id ::= <c stype identifier>
// aexpr ::= <num> | aexpr + aexpr | aexpr - aexpr | aexpr / aexpr
//         | aexpr * aexpr |
//
// bexpr ::= true | false | bexpr = bexpr | bexpr <= bexpr | bexpr >= bexpr
//         | bexpr < bexpr | bexpr > bexpr | bexpr or bexpr | bexpr and bexpr
//
// cexpr ::= new [aexpr] | x [aexpr] | nil
// valexpr ::= aexpr | bexpr | chunkexpr | id
// stmt ::= skip | id := valexpr | stmt; stmt
//        | if bexpr then stmt else stmt
//        | do stmt end | while b do stmt end

// clang-format off
enum Symbol {
    IDENTIFIER, ADD, SUB, DIV, MUL, BOOLVAR, NUMVAR, EQ, LE, GE, LT, GT, OR,
    AND, NOT, NEW, INDEX, SKIP, ASSIGN, SEQ, IFTHENELSE, BLOCK, WHILE
};

// endomorphic binop or unary.
template <typename T>
struct Binop : T { std::unique_ptr<T> left; std::unique_ptr<T> right; };
template <typename T> struct Unary : T { std::unique_ptr<T> value; };

// sum types.
struct Expr { virtual Symbol sym() const; };
struct Stmt { virtual Symbol sym() const; };
struct AExpr : Expr {};
struct BExpr : Expr {};
struct CExpr : Expr {};

#define DEC_SYM(s) Symbol sym() const override { return s; }

struct Id final : Expr {
    DEC_SYM(IDENTIFIER);
    std::string name;
};

struct Num final : AExpr {
    DEC_SYM(NUMVAR);
    int64_t value;
};

struct Bool final : AExpr {
    DEC_SYM(BOOLVAR);
    bool value;
};

struct Add final : Binop<AExpr> { DEC_SYM(ADD); };
struct Sub final : Binop<AExpr> { DEC_SYM(SUB); };
struct Div final : Binop<AExpr> { DEC_SYM(DIV); };
struct Mul final : Binop<AExpr> { DEC_SYM(MUL); };
struct Eq final : Binop<BExpr> { DEC_SYM(EQ); };
struct Le final : Binop<BExpr> { DEC_SYM(LE); };
struct Ge final : Binop<BExpr> { DEC_SYM(GE); };
struct Lt final : Binop<BExpr> { DEC_SYM(LT); };
struct Gt final : Binop<BExpr> { DEC_SYM(GT); };
struct Or final : Binop<BExpr> { DEC_SYM(OR); };
struct And final : Binop<BExpr> { DEC_SYM(AND); };
struct Not final : Unary<BExpr> { DEC_SYM(NOT); };
struct New : CExpr { DEC_SYM(NEW); std::unique_ptr<AExpr> size; };
struct Index : CExpr { DEC_SYM(INDEX);  std::unique_ptr<AExpr> index; };
struct Skip : Stmt { DEC_SYM(SKIP); };

struct Assign : Binop<Stmt> { Symbol ASSIGN; };
struct Seq : Binop<Stmt> { Symbol Seq; };
struct IfThenElse : Stmt {
    Symbol IFTHENELSE;
    std::unique_ptr<BExpr> test;
    std::unique_ptr<Stmt> then_clause;
    std::unique_ptr<Stmt> else_clause;
};

struct Block : Unary<Stmt> { Symbol BLOCK; };
struct While : Stmt {
    Symbol While;
    std::unique_ptr<BExpr> test;
    std::unique_ptr<Stmt> loop_body;
};
// clang-format on

class Parser {
    std::string code;
    Symbol sym;
}; // namespace paser


class Eval {

    bool evaluated(Expr v) const {
        return v.sym() == NUMVAR || v.sym() == BOOLVAR;
    }

    Expr eval(Expr expression) {}

    Stmt execute(Stmt program) {}

};

class Repl {};
