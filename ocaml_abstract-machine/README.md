# Some canonical abstract machine

```
SECD machine and krivine machines represents simple `apply-eval` and `push-enter`
abstract machines for call by value and call by name semantics respectively.

Although a call by value semantics can also use push-enter model, and it seems
requires less instructions, some studies shown always use apply-eval
even it's call by need calling scheme. (Making a fast curry: push/enter vs.
eval/apply for higher-order languages by Simon Marlow and Simon Peyton Jones)
```
