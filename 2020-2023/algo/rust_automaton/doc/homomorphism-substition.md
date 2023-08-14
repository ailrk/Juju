# Homomorphism

A homomorphism between langauge is a function h: ∑* → Δ* and h(ε) = ε, h(xy) = h(x)h(y).

Some examples:
```
h(0) = 12
h(1) = 010

h(0110) = h(0) h(1) h(1) h(0)
        = 1201001012
```

__[thm1]__ Regular languages are closed under homomorphism.
__[thm2]__ Context free langauges are closed under homomorphism.

# Substitution

Homomorphism is somewhat restricted. Consider `h(11) = 123`. What does this mean? `h(11) = h(1) h(1)`, so we should construct `h(11)` from `h(1)` separatedly, but here we define `h(11)` as it is. This leads to a generalization of homomorphism: substitution of strings.

__[def]__ `substitution`: s: ∑*→L, and s(ε)={ε}, s(xy)=s(x)s(y).

It works like a formalism of macro.


__[thm3]__ Regular languages are closed under subsitution.
