#include "pl0_lex.h"
#include <iostream>

// Shift reduce parser:
//

// - build the syntax tree bottom up, right most derivation.
// - generate a post order traversal of the ast
// - can describe all deterministic context free grammar

static Symbol sym;
static Symbol *code;

void nextsym(void) {
    static int pos = 0;
    while (code[pos] != eof) ++pos;
}

void error(char const msg[]) { fprintf(stderr, msg); }
