#include "regex.c"

char buffer[2048];
int i = 0;

void push_buffer(char n, char *buffer, int *i) {
    if (n != '^') {
        buffer[*i] = n;
        (*i)++;
    }
}

void push_buffer_cb(node *n) { push_buffer(n->s, buffer, &i); }

void test1() {
    code = "ab*(a|c)";
    node *n = parse();

#ifdef Debug
    PRINT_NODE(":: ", n);
    preorder_traversal(n, NULL, push_buffer_cb);
    printf(buffer);
    printf("\n");
#endif

    free_node(n);
    regex_clear();
}

void test2() {
    state = 0;
    code = "ab*(a|c)";
    stack = (char *)malloc(STACK_SZ);
    sp = stack;
    node *n = parse();

    preorder_traversal(n, mark_node, build_from_node);

#ifdef Debug
    print_table();
    print_fnode(final_states);
#endif
    regex_clear();
}

#define MATCH_REGEX(pat, str, expected)                                        \
    {                                                                          \
        int m = regex(pat, str);                                               \
        printf("%20s, %20s result: %d, expected: %d\n", pat, str, m,           \
               expected);                                                      \
    }

void test3() {

    MATCH_REGEX("ab*(a|c)", "aba", 1);
    MATCH_REGEX("ab*(a|c)", "abbbbbba", 1);
    MATCH_REGEX("ab*(a|c)", "abbbbc", 1);
    MATCH_REGEX("ab*(a|c)", "abbbbd", 0);

    MATCH_REGEX("ab(a*c|cd)", "abaac", 1);
    MATCH_REGEX("ab(a*c|cd)", "abaacd", 0);

    MATCH_REGEX("aa|bb*", "aa", 1);
    MATCH_REGEX("aa|bb*", "bbbb", 1);

    // TODO some errors here.
    MATCH_REGEX("(a|b)*", "a", 1);
    MATCH_REGEX("(a|b)*", "aa", 1);
    MATCH_REGEX("(a|b)*", "b", 1);
    MATCH_REGEX("(a|b)*", "bb", 1);
    MATCH_REGEX("(a|b)*", "bbb", 1);
    MATCH_REGEX("(a|b)*", "aabb", 1);
    MATCH_REGEX("(a|b)*", "bbaa", 1);
    MATCH_REGEX("(a|b)*", "bababbaab", 1); // can't end with b
}

int main(void) {
    /* test1(); */
    /* test2(); */
    test3();
    return 0;
}

// TODO
// problem
//    *
//    |
//    |
//   / \
//  a   b
// doesn't work correctly.
// expected behavior:
//   preorder traverse,
//
//     *
//     |
//     .
//    / \
//   a   b
//
