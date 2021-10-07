#include <memory>
#include <stdio.h>
#include <vector>

// IMP language with a tree walk interpreter.
// The langauge is only used for testing different garbage collection
// algorithms.

// Support integer, boolean, dynamically allocated memory block called Chunk .
// There is no explicit syntax for declaring chunk. To initalize simply assign
// to cells.
//
//  Int, bool are primitive datatype allocated on the stack, chunks only
//  allocated on the heap. All chunks are boxed.
//
//  [., ., .]
//   |  |  |
//   1  2  3
//
//  Assinging a primitive to a cell in chunk will allocate the primitive on
//  heap and create a pointer in the chunk that points to it.
//    a = alloc[10]
//    a[0] := 1
//
//  Only primitives have copy semantics. Chunk has reference semantics

// Because performance is not the goal, this design makes the langauge much
// simpler.

// id ::= <c stype identifier>
// id_list := id | id, id_list | ε
// aexpr ::= <num> | aexpr + aexpr | aexpr - aexpr | aexpr / aexpr
//         | aexpr * aexpr |
//
// bexpr ::= true | false | bexpr = bexpr | bexpr <= bexpr | bexpr >= bexpr
//         | bexpr < bexpr | bexpr > bexpr | bexpr or bexpr | bexpr and bexpr
//
// cexpr ::= new [ aexpr ] | id [ aexpr ] | nil
// fexpr ::= id ( varexpr_list )
// valexpr ::= aexpr | bexpr | chunkexpr | id | stmt; expr | fexpr
// valexpr_list ::= valexpr | varexpr, valexpr_list | ε
// stmt ::= skip | abort | id := valexpr | stmt; stmt
//        | if bexpr then stmt else stmt
//        | do stmt end | while b do stmt end
//        | def id ( id_list ) stmt

namespace syntax {

// clang-format off
enum Symbol {
    IDENTIFIER, ADD, SUB, DIV, MUL, BOOLVAR, NUMVAR, EQ, LE, GE, LT, GT, OR,
    AND, NOT, NEW, ESEQ, INDEX, SKIP, ABORT, ASSIGN, SEQ, IFTHENELSE,
    BLOCK, WHILE,
    CALL, DEFUNC
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
struct FExpr : Expr {};

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
struct Call : FExpr {
    DEC_SYM(CALL);
    std::vector<std::unique_ptr<Expr>> parameters;
};
struct Defunc : Stmt {
    DEC_SYM(DEFUNC);
    std::vector<std::unique_ptr<Stmt>> body;
};

// a statement block that treated as an expression.
struct SeqExpr final : Expr {
    DEC_SYM(ESEQ);
    std::unique_ptr<Stmt> stmts;
    std::unique_ptr<Expr> expr;
};

struct Skip : Stmt { DEC_SYM(SKIP); };
struct Abort: Stmt { DEC_SYM(ABORT); };

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
}; // namespace syntax

// the langauge simple enought that we don't even need an IR.
class Parser {
    std::string code;
    syntax::Symbol sym;
    std::string::iterator code_ptr;

    void nextsym() {}

  public:
    Parser(std::string code)
        : code(std::move(code))
        , code_ptr(std::begin(code)) {}

}; // namespace paser

void gensym(char *label) {
    static int id = 0;
    sprintf(label, "#label_%d", id++);
};

// clang-format off
namespace irtree {
// convert from AST to IR1. Because the langauge is so simple, the ir 1 is
// roughly the same, only differences are implementing some primitives and
// transform control flow to test and jump.

// We directly transfer ast to ir1's ast.
// binop ::= ADD | SUB | MUL | DIV | AND | OR | NOT
// relop ::= EQ | NE | LT | GT | LE | GE
// expr ::= NUM <int> | ID id | Binop binop a b | MEM expr
//        | CALL expr, expr... | ESEQ stmt expr | NEW expr
// stmt := MOVE expr expr | EXPR expr | JUMP expr ids
//       | CJUM relop expr expr id | SEQ stmt stmt
//       | Label id

enum IRTREE {
   IDENTIFIER, BOO, NUM,
   ADD, SUB, DIV, MUL,
   EQ, LE, GE, LT, GT, OR, AND, NOT,
   NEW, ESEQ, INDEX, SKIP, ABORT, MOVE, SEQ, EXPR,
   CALL
};

// clang-format on
} // namespace irtree

class IRTreeGen {};

template <typename BackEnd>
class Eval {

    /* bool evaluated(Expr const &v) const { */
    /*     return v.sym() == NUMVAR || v.sym() == BOOLVAR; */
    /* } */

    /* Expr eval(Expr &expression); */
    /* Stmt execute(Stmt &program); */
};

template <typename T>
struct backend_traits {};

// base line tree walk interpreter.
class TreeWalk {};

// compile to C.
class CCodegen {};
