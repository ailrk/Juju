#include <stdio.h>

// monotonic queue
// given a int sequence, find the min/max element within a window k


// Notes on monotonic queue:
// 1. No matter what happens, the new element will always be enqueued. 
// 2. For monotonic decreasing, if new next element is bigger than the tail of the queue,
//    we need to clean the queue to maintain the invariant.
//
// 3. If the next element is smaller, we directly enqueue because it doesn't affect the
//    monotonic decreasing property.
// 4. And that's it. This maintain the property of the window, so the first element in
//    the queue is the biggest, the last is the smallest.

// Notes on C techniques
// C is the languge of array.
//
// Common technique with C is to simulate everything with array only.
// In this example q is an array of index of elements in a.
// We don't copy, only reference. Basically we're working with an array of pointers.

// Note
// gdb script is very useful. You can write some debugging logic and have it output 
// to a file, so you can analyse the output just like you're printing them in the
// program.

#define MAXN 1000100
int q[MAXN], a[MAXN];
int n, k;



static int q_last(int tail) { return a[q[tail]]; }
static int q_first(int head) { return a[q[head]]; }
static void q_enque(int *tail, int v) { q[++*tail] = v; }
static void q_clear(int head, int *tail) { while (*tail > head) (*tail)--; }

void q_print(int head, int tail) {
    printf(": ");
    for (int i = head; i <= tail; ++i) {
        printf("%d ", a[q[i]]);
    }
    printf("\n");
}
 

void getmin() {
    int qhead = 0, qtail = -1;

    q_enque(&qtail, 0);
    for (int i = 1; i < k; ++i) {
        if (a[i] >= q_last(qtail)) q_clear(qhead, &qtail);
        q_enque(&qtail, i);
        q_print(qhead, qtail);
    }

    for (int i = k; i < n; ++i) {
        if (a[i] >= q_last(qtail)) q_clear(qhead, &qtail);
        q_enque(&qtail, i);
        while (q[qhead] <= i - k) qhead++;
        q_print(qhead, qtail);
    }
}



int main(void)
{
    scanf("%d", &n);
    scanf("%d", &k);
    for (int i = 0; i < n; ++i) scanf("%d", &a[i]);
    printf("\n");
    getmin();
    printf("\n");
}
