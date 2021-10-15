use std::collections::HashSet;
// Regular languages have minimal automaton (canonical dfa) that accept it.
// we can convert any dfa to it's canonical form by triming unecssary states.

struct DFA {
    q: HashSet<char>,
    semga: HashSet<char>,
    delta: fn(char, char) -> Option<char>,
    q0: char,
    f: HashSet<char>,
}

impl DFA {

    // O(n + m) where n is number of states m is number of transitions.
    fn get_unreachable(&mut self) -> HashSet<char> {
        let mut front_states = HashSet::from([self.q0]);
        let mut reachable_states = HashSet::from([self.q0]);

        while !front_states.is_empty() {
            let mut temp = HashSet::new();
            for q in front_states.iter() {
                for c in self.semga.iter() {
                    if let Some(s) = (self.delta)(*q, *c) {
                        temp.insert(s);
                    }
                }
            }
            front_states = &temp - &reachable_states;
            reachable_states.extend(&front_states);
        }

        &self.q - &reachable_states
    }

    //   Hopcroft is an algorithm to merge nondistinguishable states. It uses
    //   partition refinement. That is, keep partitioning nondistinguishable
    //   states into the same group until no more. nondistinguishable state left.
    fn hopcroft(&mut self) {
        todo!()
    }
}
