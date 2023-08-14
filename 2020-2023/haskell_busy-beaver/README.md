# Busy beaver
For a turing machine with binary alphabet and state size n, find a terminating program that produces the most output. Such turning machine is called busy beaver.

## Goal
in a BB-n game, finding the turing machine M that give us the longest 1 on the tape.

```
-- Trace the path of 2 symbol 3 states busy beaver
-- [state: 'a', tape: "_________________________________"
-- ,state: 'b', tape: "________________1________________"
-- ,state: 'a', tape: "________________11_______________"
-- ,state: 'c', tape: "________________11_______________"
-- ,state: 'b', tape: "_______________111_______________"
-- ,state: 'a', tape: "______________1111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'a', tape: "_____________111111______________"
-- ,state: 'c', tape: "_____________111111______________"
-- ,state: '!', tape: "_____________111111______________"]
```

## Remarks
* Determining whether an arbitrary turing machine is a busy beaver is undecidable.
* The amount of BB-n machine is finite, and a subet of them will halt.
* When n is small, we can try all possible machines and compare their results.
* If a specific machine doesn't halt, we can try to prove it for that specific case.
* But there is no general algorithm that if we give a BB-n game, it tells us the busy beaver. It will not halt on non halting machines.

## Hilbert's program and godel's incompleteness theory

##### Earily foundational formalisms
Naive set theory was developed by Cantor for analysing infinite sets. Cantor's theorem states that for any set S, the power set of A has strictly greater cardinality then A itself. To prove it for finite set, we can simply count the number of subsets S and P(S) has. (We know if `|S| = n`, then |`P(x)| = 2ⁿ > n ∀n >= 0`) However, the interesting problem lies on whether it's also true for infinite sets.

Bertran Russell proved naive set theorems was inherently inconsistent. The contradiction rise from the state: if set x in the set of all sets S that doesn't contain itself, is x in S? `{x ∈ S : x ∉ S}`. This is an earily discussion of the consistency of formal system.

##### Hilbert's motivation

David Hilbert believed that it's curcial to build mathematics in a consistent axiomatic foundation: everything derived from the axiomatic system should never lead to a contradiction. This idea was originated from the thought: there are lots of mathematic theorems derived from the same set of axioms, can we be sure that two theorems will not eventually lead to a contradiction?

* Consistency: Proofs without contradiction can be obtained in the formalism.
* Completeness: The formalism can be used to prove all true mathematical statements.
* Decidability (Entscheidungsproblem): There exists an algorithm decides whether a statement is a theorem

Particularly, decidability means that if we know an algorithm is a theorem, we we can inductively prove all theorems by trying all axioms until we find a proved for it.

##### Godel's incompleteness theory

## Computability

##### Effectively computable and turing computable

##### Turing church thesis.

##### Non computable

## Halting problem

## Cantor's diagnoal argument

## Rice Theorem

## Busy beaver is not computable

##### Dynamic proof

##### Static proof

## Oracle machine
- Turing Church thesis is never proved or disproved. We accept it by default.
- Reason to accept turing church thesis
- Nothing stop us to define a machine that anwser problems that is non computable,
- Oracle machine as a hypothetical machine.

oracle turing machine is able to query a oracle about any decision problem or function, problem, and the oracle is able to give answer in one step, even if the problem is non computable. Oracle machine cannot ask the oracle about it's own halting property, to do that, we need a higher level oracle machine on top of the current machine. This leads to the so called `arithmetic hierachy`.

We cannot really have an oracle, but we can somehow simulate one with neuro network. A trained model can give an answer of a decision problem base on the input vector within constant time, although there is a probability for the answer the oracle in our case is a trained neural network with back propagation algorithm

##### Arithmetic Hierachy

## ANN

### Percepton

Percepton is the simplest form of neural network, it consists only one layer of input nodes and one layer of output nodes. Each output node is connected from all input nodes, and the entire perceptron forms a directed acyclic graph. The value of an output node is the value of linear combination of input nodes mapped by an activation function. Output layer forms a vector in the solution space that corresponds to the prediction of the model.

Nodes in the output layer are connected by edges from input nodes, each edge has an assoicated weight, and each output node has a bias. When training on labeled data, we can access the degree of error of the current model by computing the difference between output and label with a loss function. Training a perceptron involves minimizing the value of the loss function by tweaking the value of weights and bias of each edges. Each time we learn on a new data, we compute a output and it's loss, then tweak weights and bais so the new loss decreases. This process is repeated for all input data, and each learning process captures some pattern of the dataset to make the model more accurate overall. Like other machine learning models, percepton establish an estimation of the real correlation between data and label. The more data we can work with, the more accurate the model will be.

The process of an entire step works like this:

* Input data as a feature vector
* Each output node evaluates to the linear combination of input feature over weights of edges and bais the output node is assigned to.
* Compute the loss of the result (delta rule for perceptron)
* Base on the loss and some strategy to update weights and bais, so the new loss is smaller.

##### Linearly separable pattern
Feature vetors scattered across a vector space. If we give each feature a label, and there exists a hyperplane that separates all vectors of one label to one side, and the rest to the other, then the data is linearly separable. Linear problems are linearly separable. For example, the problem of determing whether n > 0 for all integer n. Feature is a 1 dimensional vector n, and output is a 2d vector [t f], where t represents `n > 0`, f represents `n <= 0`. This model is linearly separable: all vectors above y = 0 are in one side and the rest are in the other side. On the other hand `xor` function is an example of a non linearly separable problem. Precentron cannot learn a non linearly separable model.

### Universal approximation theorem

### feedforward neural network

### Recurrent neural network

### Activation functions

```
# Hard limit function:
ϕhlim(v) = {
  1 for v >= 0,
  0 for v < 0
}

# Piecewise linear function:
ϕpwl(v) {
  1       for v >= 1/2
  v + 1/2 for -1/2 < v < 1/2
  0       for v <= -1/2
}

# Sigmoid function:
ϕsig(α) (v) = 1 / (1 + exp(-αv))

# Rectified linear unit (ReLu):
max(0, x)
```
### Backpropagation

##### Loss function

##### Gradient decent
