#include <ctype.h>
#include <stdio.h>
typedef enum {
    ident,
    number,
    lparen,
    rparen,
    times,
    slash,
    plus,
    minus,
    eql,
    neq,
    lss,
    leq,
    gtr,
    geq,
    callsym,
    beginsym,
    semicolon,
    endsym,
    ifsym,
    whilesym,
    becomes,
    thensym,
    dosym,
    constsym,
    comma,
    varsym,
    procsym,
    period,
    oddsym,
    eof
} Symbol;

int same_str(char const *stream, char const msg[]) {
    while (*msg != '\0')
        if (*stream++ != *msg++)
            return false;
    return true;
}

// no boundary check!
void lexer(Symbol *code, char const text[]) {
    for (int i = 0; text[i] != '\0'; ++i) {
        if (isspace(text[i]))
            continue;
        if (isdigit(text[i])) {
            *code++ = number;
        } else if (text[i] == '(') {
            *code++ = lparen;
        } else if (text[i] == ')') {
            *code++ = rparen;
        } else if (text[i] == '+') {
            *code++ = plus;
        } else if (text[i] == '-') {
            *code++ = minus;
        } else if (text[i] == '*') {
            *code++ = times;
        } else if (text[i] == '/') {
            *code++ = slash;
        } else if (text[i] == '=') {
            *code++ = eql;
        } else if (text[i] == '#') {
            *code++ = neq;
        } else if (text[i] == ';') {
            *code++ = semicolon;
        } else if (text[i] == ',') {
            *code++ = comma;
        } else if (text[i] == '>') {
            *code++ = gtr;
        } else if (text[i] == '<') {
            *code++ = leq;
        } else if (same_str(text + i, ">=")) {
            *code++ = geq;
        } else if (same_str(text + i, "<=")) {
            *code++ = leq;
        } else if (same_str(text + i, "odd")) {
            *code++ = oddsym;
        } else if (same_str(text + i, ":=")) {
            *code++ = becomes;
        } else if (same_str(text + i, "call")) {
            *code++ = callsym;
        } else if (same_str(text + i, "begin")) {
            *code++ = beginsym;
        } else if (same_str(text + i, "end")) {
            *code++ = endsym;
        } else if (same_str(text + i, "if")) {
            *code++ = ifsym;
        } else if (same_str(text + i, "then")) {
            *code++ = thensym;
        } else if (same_str(text + i, "do")) {
            *code++ = dosym;
        } else if (same_str(text + i, "const")) {
            *code++ = constsym;
        } else if (same_str(text + i, "var")) {
            *code++ = varsym;
        }
    }
}
