#include "regex.h"
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define Debug
#define MAX_STATE_NUM 128
#define MAX_SYM_NUM 128
#define STACK_SZ 128

char const *code; // the input regex
char sym;         // current symbol, for parsing only.
int pos;

char *stack; // stack for building the table
char *sp;    // stack pointer.
int state;   // current state.

// This regualr expression library supports matching 128 long
// ascii string.
//
// A regular expression engine is essentially a regular expression to
// state machine compiler. Given a langauge ∑ and a set of states Q,
// we have transition function δ: ∑ × Q → Q. That is, the state machine
// is a 2d table indexed by the current state and current symbol.
//
// nd Array is actually curried functions, fully applied nd array gives
// us the final mapping. In this case is the next state the machine goes
// to.
//
// So another view of regex engine is that we're building the arity 2
// mapping functions before hand so we can use it afterwards.
char table[MAX_SYM_NUM][MAX_STATE_NUM];

// The size of final state is unkown untill we get the reuslt, so the final
// state is a linked list.
struct fnode {
    int state;
    struct fnode *next;
} * final_states;

struct fnode *new_fnode(int state, struct fnode *next) {
    struct fnode *n = (struct fnode *)malloc(sizeof(struct fnode));
    n->state = state;
    n->next = next;
    return n;
}

#define FINAL_STATE_UPDATE(state)                                              \
    if (final_states)                                                          \
        final_states->state = state;                                           \
    else                                                                       \
        final_states = new_fnode(state, NULL);

#define FINAL_STATE_PUSH(state) final_states = new_fnode(state, final_states);

#define FINAL_STATE_CLEAR()                                                    \
    if (final_states)                                                          \
        free_fnode(final_states);                                              \
    final_states = new_fnode(0, NULL);

int member(int state, struct fnode *n) {
    struct fnode *p = n;
    while (p) {
        if (p->state == state)
            return 1;
        else {
            if (!p->next)
                break;
            else
                p = p->next;
        }
    }
    return 0;
}

void free_fnode(struct fnode *fn) {
    if (!fn)
        return;
    if (fn->next)
        free_fnode(fn->next);
    free(fn);
}

// clear regex state.
void regex_clear() {
    code = NULL;
    state = 0;
    sym = 0;
    pos = 0;

    if (stack)
        free(stack);
    stack = NULL;

    sp = NULL;
    memset(table, 0, MAX_SYM_NUM * MAX_STATE_NUM * sizeof(char));
    FINAL_STATE_CLEAR();
}

// init regex state
void regex_init(char const *pat) {
    state = 0;
    FINAL_STATE_CLEAR();

    pos = 0;
    code = pat;
    if (stack)
        free(stack);
    stack = (char *)malloc(STACK_SZ);
    sp = stack;
}

// define node inside so it doesn't leak outside.
// To handle regex input we first parse it into an ast. For implicity we use
// one node type for all types of nodes. For unary operator, one child will
// left empty (which one depends on cases).
// For leaf node, both will left empty.
typedef struct node_ {
    char s;
    struct node_ *left;
    struct node_ *right;
} node;

node *new_node(char s, node *left, node *right) {
    node *n = (node *)malloc(sizeof(node));
    n->s = s;
    n->left = left;
    n->right = right;
    return n;
}

#ifdef Debug
void print_table() {
    char n;
    // readable ascii start from 33 to 127
    printf("    ");
    for (int i = 0; i < 10; ++i) {
        printf("%d ", i);
    }
    printf("\n");
    // for (int i = 0; i < MAX_STATE_NUM; ++i) {
    for (int i = 97; i <= 'c'; ++i) {
        printf("%c | ", i);
        // for testing we won't use more than 10 states.
        // for (int j = 0; j < MAX_SYM_NUM; ++j) {
        for (int j = 0; j < 10; ++j) {
            n = '0' + table[i][j];
            printf("%c ", n);
        }
        printf("\n");
    }
}

void print_fnode(struct fnode *fn) {
    struct fnode *p = final_states;
    while (p) {
        printf("%d ", p->state);
        p = p->next;
    }
    printf("\n");
}

void print_node(node *n) {
    if (n == NULL)
        return;
    printf("(%c ", n->s);
    if (n->left)
        print_node(n->left);
    else
        printf(" _ ");
    if (n->right)
        print_node(n->right);
    else
        printf(" _ ");
    printf(") ");
}

#define PRINT_NODE(msg, n)                                                     \
    printf(msg "\n");                                                          \
    print_node(n);                                                             \
    printf("\n");
#endif

// pass preprocess and cb to customize action to perform on preorder
// traversal.
void preorder_traversal(node *n, void (*preprocess)(node *),
                        void (*cb)(node *)) {
    if (preprocess)
        preprocess(n);
    if (n->left)
        preorder_traversal(n->left, preprocess, cb);
    if (cb)
        cb(n);
    if (n->right)
        preorder_traversal(n->right, preprocess, cb);
}

void free_node(node *n) {
    if (n->left)
        free_node(n->left);
    if (n->right)
        free_node(n->right);
    free(n);
}

void error(char const msg[]) {
    fprintf(stderr, msg);
    fprintf(stderr, "\n");
}

void nextsym() {
    if (code[pos] != '\0')
        sym = code[pos++];
    else
        sym = '\0';
}

int accept(char s) {
    if (sym == s) {
        nextsym();
        return 1;
    } else
        return 0;
}

// accept an char in range a-z, return the accepted char.
char accept_alpha() {
    if (sym >= 'a' && sym <= 'z') {
        char n = sym;
        nextsym();
        return n;
    } else
        return 0;
}

int expect(char s) {
    if (accept(s))
        return 1;
    error("expect: unexpected token");
    return 0;
}

// EBNF for regex:
//   factor = char | "(" expression ")"
//   term = factor ["*"]
//   term1 = term { term }
//   expression = term1 { "|" term1 }
//
// note: regular expression has two operations, . and |
// . is a non comutative monoid, and | is a group. let empty string be
// the identity then it forms a non conmutative ring.
//
// Here we omit . operator, so there is no need to define . as a node
// in the syntax. However we still need to convert it into an ast node.

// A recursive decent parser for regex.
node *expression();

node *factor() {
    if (accept('(')) {
        node *e;
        e = expression();
        expect(')');
        return e;
    } else {
        char s;
        s = accept_alpha();
        if (s > 1) {
            node *p = new_node(s, NULL, NULL);
            return p;
        } else {
            printf("factor error %c\n", sym);
            error("factor: syntax error");
            nextsym();
        }
        return NULL;
    }
}

node *term() {
    node *f;
    f = factor();
    if (accept('*')) {
        node *klenee_star = new_node('*', NULL, f);
        return klenee_star;
    }
    return f;
}

node *term1() {
    node *t1, *t2, *o;
    o = term();
    while (isalpha((int)sym) || sym == '(') {
        t1 = o;
        o = new_node('.', NULL, NULL);
        t2 = term();
        if (t2) {
            o->left = t1;
            o->right = t2;
            t1 = o;
        } else {
            error("term1: term { term }");
            nextsym();
        }
    }
    return o;
}

node *expression() {
    node *t1, *t2, *o;
    o = term1();
    while (accept('|')) {
        t1 = o;
        o = new_node('|', NULL, NULL);
        t2 = term1();
        if (t2) {
            o->left = t1;
            o->right = t2;
            t1 = o;
        } else {
            error("expression: error for term1 { [\"|\"] term1 } ");
            nextsym();
        }
    }
    return o;
}

node *parse() {
    nextsym();
    return expression();
}

// callbacks for preorder traversal.

// A state machine table is a transtion function indexed by symbol and
// state. Thus transition : Sym -> State -> State
//   0 1 2 3 4
// a 1 0 3 0 0
// b 0 2 2 0 0  for regex ab*(a|c)
// c 0 0 4 0 0
//
//                .
//              /   \
//          0 .       |
//           / \     / \
//          a   *   a   c
//          1   |   3   4
//              b
//              2

// mark nodes when the first time we enter it.
// note in preorder traversal, each node will be visited exactly twice.
// The first time we visit a node we can staff some information into the stack,
// and fetch it in the next time we hit it again.
// information we care about are the current symbol and current state.
// ( -- saved_state | )
void mark_node(node *n) {
    if (n->s == '|') {
        *sp++ = (char)state; // record the current state when hit a |
        *sp++ = '|';
    }

    if (n->s == '*') {
        *sp++ = (char)state;
        *sp++ = '*';
    }

    if (n->s == '.')
        *sp++ = '.';
}

void build_from_node(node *n) {
    char sym;
    static struct {
        int typ;
        int val;
    } sig = { 0, 0 };

    if (n->s == '|') { // hit | the second time.
        // note: | has the shape
        //    |
        //   / \
    //  A   B
        // let state before a be x. then state after a is x + 1, after b is
        // x + 2.
        // we save the state x on the stack, and when the second time we
        // visit |, where we are about to visist b, we can signal b the
        // state s.
        sig.typ = *--sp;
        sig.val = *--sp;

        if (sig.typ != '|')
            error("build_from_node: stack error on |");
    } else if (n->s == '*') {
        sig.typ = *--sp;
        sig.val = *--sp;

        if (sig.typ != '*')
            error("build_from_node: stack error on *");
    } else if (n->s == '.') {
        sig.typ = *--sp;

        if (sig.typ != '.')
            error("build_from_node: stack error on .");
    } else if (isalpha(n->s)) {
        sym = n->s;
        if (sig.typ == '*') {
            // note: * in the ast has the shape
            //      *
            //     / \
      //    _   n
            //
            // where n is the right child, so * will be hit twice before we
            // reach n. let state before n be x. after be y. without * it should
            // naturally be:
            //  table[n][x] = y
            // but with * we should also add entry
            //  table[n][y] = y
            // so we create a loop.
            // y is saved on the stack.
            int prev_state = state++;
            table[sym][sig.val] = state;
            table[sym][state] = state;

            // the final state update when we traverse along tree. If there is a
            // new state we should update the final state.
            FINAL_STATE_UPDATE(state);
        } else if (sig.typ == '|') {
            table[sym][sig.val] = ++state;
            FINAL_STATE_PUSH(state);
        } else {
            int prev_state = state++;
            table[sym][prev_state] = state;
            FINAL_STATE_UPDATE(state);
        }

        memset(&sig, 0, sizeof(int) * 2);
    }
}

void compile_regex(char const pat[]) {
    node *expr;
    regex_init(pat);
    expr = parse();

#ifdef Debug
    print_node(expr);
    printf("\n");
#endif

    preorder_traversal(expr, mark_node, build_from_node);
    free_node(expr);
}

int regex(char const pat[], char const str[]) {
    int match;
    compile_regex(pat);

    {
        state = 0;
        for (char const *p = str; *p != '\0'; ++p) state = table[*p][state];
        match = member(state, final_states);
    }

#ifdef Debug
    print_table();
    print_fnode(final_states);
#endif

    regex_clear();
    return match;
}
