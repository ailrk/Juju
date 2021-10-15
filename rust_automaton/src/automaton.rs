use std::collections::HashMap;
use std::collections::HashSet;

type Q = char;
type S = char;
type Z = char;

#[derive(Debug, Default, Clone)]
pub struct DFA {
    pub q: HashSet<Q>,
    pub semga: HashSet<S>,
    pub delta: HashMap<(Q, S), Q>,
    pub q0: Q,
    pub f: HashSet<Q>,
}

#[derive(Debug, Default, Clone)]
pub struct NFA {
    pub q: HashSet<Q>,
    pub semga: HashSet<S>,
    pub delta: HashMap<(Q, S), Q>,
    pub q0: Q,
    pub f: HashSet<Q>,
}

#[derive(Debug, Default, Clone)]
pub struct PushDown {
    pub q: HashSet<Q>,
    pub semga: HashSet<S>,
    pub gamma: HashSet<Z>,
    pub delta: HashMap<(Q, Option<S>, Z), (Q, Vec<Z>)>,
    pub q0: Q,
    pub z: Z,
    pub f: HashSet<Q>,
}

#[derive(Debug, Default, Clone)]
pub struct TuringMacine {
    pub q: HashSet<Q>,
    pub b: S,
    pub semga: HashSet<S>,
    pub delta: HashMap<(Q, Option<S>), (Q, S, bool)>,
    pub q0: Q,
    pub z: Z,
    pub f: HashSet<Q>,
}
