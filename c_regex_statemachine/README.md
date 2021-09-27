# Comile regular expression to a table

A regular langauge translates to a finite state machine. We have simpler deterministic finite automaton (DFA) and more general nondeterministic finite automaton(NFA), but because NFA can be converted into DFA, we can think about finite state machine in DFA only.

DFA can be defined as a tuple (∑, Q, q₀, qf, σ). We have ∑ the alphabet, Q a set of possible statesm q₀ the initial state, and qf is the final state, and finally the transition function σ.

Before talking about state machine, it's helpful to first think about what are states. We can ignore more thinking, but focus on what do we call `states` when programming. A typical example I always think about is to light an LED. You write some C program, it calls some gpio library. The library sends a singal when you pass a 1, or it does nothing when you pass a 0. You define this parameter somewhere in the file and call it `int led_state`. It's naturall to call such a thing state, but it's really just an integer. What makes it a state though? It's because at this moment the value of this variable is something, but at the next moment it can change to something else. Over time, the value of the state changes base on some rules.

The notion of stepping over time is very generic. What does time means? In real world it's just the time around us, but for a piece of hardware it might means the clock cycle. A cpu won't do anything unless a clock signal pass through it, so we can savely ignore anything happen between clock cycles: nothing happen there anyway. Thus a state is a value that can change from this cycle to the next cycle. This is interesting because in this case the state still changes over time, but regards to some artificial time we give it instead of the wall clock time.

This idea can be further abstracted away. We can say that a state is just a value changes between steps. Single core programs take one step at a time, and there is nothing happening between steps. This step can be a block of statements, a function call, or anything atomic. e.g If steps are funtion calls, function calls becomes the clock singal. In this case, a state is any variable that can change it's value after a step is performed.

One way to simulate states in functional lanaguages is to pass the state as a parameter, and return a tuple of the new state and the return value. Because function calls are clock signals, state change only happens after a function call is finished.

So with all this being said, state is just something that can change regard to some clock singals, or after each steps.

Back to state machine

So why state machine can be used as a design tool? It abstract away cluttering local states and only shows you the important state transitions. Then you can write your program with the state transition in mind, as long as you implmement the digram faithfully, no matter how complicated each component is, it will work as expected.

A regular expression describes finite state machine. Our clock now is the token stream. Whenever we eat a symbol the time is one step ahead. The value of the next state is determined by the value of the current state and the current symbol value. `δ: ∑ × Q → Q`.

A state machine is just a table. If we look at the transitio function, `δ: ∑ × Q → Q`, it's a table indexed by two values. One is the current symbol, and the other is the current state. More specifically, σ is just a simple mapping. There is no for loop, no blocks, nothing. It's a simple pure function that maps from a tuple of value to another value. So it's static, we can draw the definition of a transition funciton as a 2d table. The following is an example table for the regular expression `ab*(a|c)`.

```
   0 1 2 3 4
  +------------ (state)
a | 1 0 3 0 0
b | 0 2 2 0 0   regex ab*(a|c)
c | 0 0 4 0 0
  +------------
  |
(symbol)
```

```
A state machine table is a transtion function indexed by symbol and
state. Thus transition : Sym -> State -> State
               .
             /   \
         0 .       |
          / \     / \
         a   *   a   c
         1   |   3   4
             b
             2
```

I like tables. Tables are powerful tool for remembering things.

Fun fact, tables are just curried functions.

Interesting enough, grammar for regular expression is context free, so you at least need a recursive descent parser to parse it.

The execution of the state machine is trivial. It's just a for loop loop over the input string.

The state and the symbol together
