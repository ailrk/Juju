use std::collections::HashSet;
use super::automaton;
// Regular languages have minimal automaton (canonical dfa) that accept it.
// we can convert any dfa to it's canonical form by triming unecssary states.

// fn hopcroft(&mut self) {

impl automaton::DFA {

    fn get_unreachable(&self) -> HashSet<char> {
        let mut front_states = HashSet::from([self.q0]);
        let mut reachable_states = HashSet::from([self.q0]);
        while !front_states.is_empty() {
            let mut temp = HashSet::new();
            for q in front_states.iter() {
                for c in reachable_states.iter() {
                    if let Some(s) = self.delta.get(&(*q, *c)) {
                        temp.insert(*s);
                    }
                }
            }
            front_states = &temp - &reachable_states;
            reachable_states.extend(&front_states);
        }
        &self.q - &reachable_states
    }

    fn merge_nondistinguishable(&self) -> HashSet<char> {

        todo!()
    }
}
