use super::single_linked_list::*;
use std::mem::transmute;

// Extensible hashtable. (Ronald Fagin)
// 1. Treat hash as bit string
// 2. use a trie for page lookup.
// Use only the first n bits that's necessary to index.

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
    depth: usize,
}

impl<K, V> Page<K, V>
where
    K: Hashable + Eq + Ord,
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
        let mut dup = 0;
        for (i, (k, v)) in self.bucket.iter().enumerate() {
            if *k == key {
                dup = i;
                break;
            }
        }
        self.bucket.remove(dup);
        self.bucket.push((key, value));
    }

    pub fn get(&self, key: K) -> Option<&V> {
        for (k, v) in self.bucket.iter() {
            if key == *k {
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        for (k, v) in self.bucket.iter_mut() {
            if key == *k {
                return Some(v);
            }
        }
        None
    }
}

pub struct HashTable<K, V> {
    gd: usize,
    pages: Vec<Page<K, V>>,
}

impl<K, V> HashTable<K, V>
where
    K: Hashable + Eq + Ord + Copy,
{
    pub fn new() -> Self {
        HashTable {
            gd: 0,
            pages: vec![],
        }
    }

    pub fn get_page(&mut self, key: K) -> &mut Page<K, V> {
        let i = key.hash() & (1 << self.gd) - 1;
        &mut self.pages[i as usize]
    }

    pub fn put(&mut self, key: K, value: V) {
        // let p = self.get_page(key);
        // p.put(key, value);
        // if p.is_full() {
        //     if p.depth == self.gd {
        //         let sz = self.pages.len() * 2;
        //         // TODO
        //     }

        //     let mut p0 = Page::<K, V>::new();
        //     let mut p1 = Page::<K, V>::new();

        //     p0.depth = p.depth + 1;
        //     p1.depth = p.depth + 1;
        // }
    }

    pub fn get(&mut self, k: K) {
        self.get_page(k).get(k);
    }
}
