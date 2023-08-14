module Oracle where

-- oracle turing machine is able to query a oracle about any decision problem
-- or function, problem, and the oracle is able to give answer in one step,
-- even if the problem is non computable.
--
-- Oracle machine cannot ask the oracle about it's own halting property, to
-- do that, we need a higher level oracle machine on top of the current
-- machine. This leads to the so called `arithmetic hierachy`.
--
-- We cannot really have an oracle, but we can somehow simulate one with neuro
-- network. A trained model can give an answer of a decision problem base on
-- the input vector within constant time, although there is a probability for
-- the answer the oracle in our case is a trained neural network with back
-- propagation algorithm


-- simple neural network
-- input layer -> hidden layer -> output layer
