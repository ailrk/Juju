use super::unsafe_queue::*;
use std::mem::transmute;

// Extensible hashtable. (Ronald Fagin)
// 1. Treat hash as bit string
// 2. use a trie for buckete lookup.

// hash
fn fnva1_64(bytes: &Vec<u8>) -> u64 {
    static OFFSET_BASIS: u64 = 0xcbf29ce484222325;
    static PRIME: u64 = 0x100000001b3;
    let mut h = OFFSET_BASIS;
    for &b in bytes {
        h = h ^ (b as u64);
        h = h * PRIME;
    }
    return h;
}

static PAGE_SIZE: usize = 8;
trait Hashable {
    fn hash(&self) -> u64;
}

impl Hashable for u8 {
    fn hash(&self) -> u64 {
        let v = vec![*self];
        fnva1_64(&v)
    }
}

impl Hashable for u64 {
    fn hash(&self) -> u64 {
        let bytes = self
            .to_be_bytes()
            .iter()
            .map(|n| n.clone())
            .collect::<Vec<u8>>();
        fnva1_64(&bytes)
    }
}

impl Hashable for String {
    fn hash(&self) -> u64 {
        let bytes = self.chars().map(|n| n as u8).collect::<Vec<u8>>();
        fnva1_64(&bytes)
    }
}

struct Page<K, V> {
    key: Option<K>,
    bucket: List<(K, V)>,
    depth: u64,
}

impl<K, V> Page<K, V>
where
    K: Eq + Ord,
{
    pub fn new() -> Self {
        Page {
            key: None,
            bucket: List::new(),
            depth: 0,
        }
    }

    pub fn is_full(&self) -> bool {
        self.bucket.iter().count() >= PAGE_SIZE
    }

    pub fn put(&mut self, key: K, value: V) {
        let dup = self
            .bucket
            .iter()
            .enumerate()
            .filter(|(i, (k, v))| key == *k)
            .map(|(i, _)| i);

    }
}

pub struct HashTable<K, V>(Vec<Page<K, V>>);

impl<K, V> HashTable<K, V>
where
    K: Hashable,
{
    pub fn new() {
        HashTable::<K, V>(vec![]);
    }
}
