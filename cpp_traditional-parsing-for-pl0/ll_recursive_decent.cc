#include "pl0_lex.h"
// Recursive descent parser:
//  is a type of parser that implements with a set of
// mututally recursive procedures, each one represents a non terminal of the
// grammar.

// Predictive parser (LL(k) grammars):
// LL(k) grammar means the parser can determine which production rule to use
//  by k token lookahead.
//  Predictive parser is a recursive decent parser that doesn't require
//  back tracking. The first k tokens determine which producition rule to use.
//
//  For some grammar multiple production rules can have the same several
//  beginning nonterminals. In this case we need to pick one and try, if failed
//  we need to go back and try to parse the other rule.
//
//  LL(k) grammar excludes all ambigious gramamrs and all gramamrs with left
//  recursions

// Backtracking
//   A recursive decent parser with backtracking will need to try each
//   produciton rule in turn. It can parser grammars that are not LL(k),
//   but can only guarantee terminates for LL(k) grammar.

// - build the syntax tree top doewn, left most derivation
// - generate a pre order traversal of the ast
// - parse restricted  context free grammar

// this parser parse without building an ast. it's essentially and
// predicate to check if a given token is in the langauge.

// It's really a beautiful scheme, tbh i feel it's clearer than parser
// combinator.

// global but it doesn't matter.
static Symbol sym;
static Symbol *code;

void nextsym(void) {
    static int pos = 0;
    if (code[pos] != eof) {
        ++pos;
        sym = code[pos];
    }
}

void error(char const msg[]) { fprintf(stderr, msg); }

// accept naturally takes next symbol.
int accept(Symbol s) {
    if (sym == s) {
        nextsym();
        return 1;
    }
    return 0;
}

int expect(Symbol s) {
    if (accept(s))
        return 1;
    error("expect: unexpected symbol");
    return 0;
}

void expression();

// factor = ident | number | "(" expression ")";
void factor() {
    if (accept(ident))
        ;
    else if (accept(number))
        ;
    else if (accept(lparen)) {
        expression();
        expect(rparen);
    } else {
        error("factor: syntax error");
        nextsym();
    }
}

// term = factor {("*"|"/") factor};
void term() {
    factor();
    while (sym == times || sym == slash) {
        nextsym();
        factor();
    }
}

// expression = [ "+"|"-"] term { ("+"|"-") term};
void expressoin() {
    if (sym == plus || sym == minus)
        nextsym();
    term();

    while (sym == plus || sym == minus) {
        nextsym();
        term();
    }
}

// condition = "odd" expression |
//             expression ("="|"#"|"<"|"<="|">"|">=") expression ;

void condition() {
    if (accept(oddsym)) {
        expression();
    } else {
        expression();
        if (sym == eql || sym == neq || sym == lss || sym == leq ||
            sym == gtr || sym == geq) {
            nextsym();
            expression();
        } else {
            error("condotion: invalid operator");
            nextsym();
        }
    }
}

// statement = [ ident ":=" expression | "call" ident
//               | "?" ident | "!" expression
//               | "begin" statement {";" statement } "end"
//               | "if" condition "then" statement
//               | "while" condition "do" statement ];

void statement() {
    if (accept(ident)) {
        expect(becomes);
        expression();
    } else if (accept(callsym)) {
        expect(ident);
    } else if (accept(beginsym)) {
        do {
            statement();
        } while (accept(semicolon));
        expect(endsym);
    } else if (accept(ifsym)) {
        condition();
        expect(thensym);
        statement();
    } else if (accept(whilesym)) {
        condition();
        expect(dosym);
        statement();
    } else {
        error("statement: syntax error");
        nextsym();
    }
}

// block = [ "const" ident "=" number {"," ident "=" number} ";"]
//         [ "var" ident {"," ident} ";"]
//         { "procedure" ident ";" block ";" } statement ;

void block() {
    if (accept(constsym)) {
        do {
            expect(ident);
            expect(eql);
            expect(number);
        } while (accept(semicolon));
    }

    if (accept(varsym)) {
        do {
            expect(ident);
        } while (comma);
        expect(semicolon);
    }

    while (accept(procsym)) {
        expect(ident);
        expect(semicolon);
        block();
        expect(semicolon);
    }
    statement();
}

void program() {
    nextsym();
    block();
    expect(period);
}
