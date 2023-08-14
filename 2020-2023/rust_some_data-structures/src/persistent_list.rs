use std::rc::Rc;

pub struct List<T> {
    head: Link<T>,
}

type Link<T> = Option<Rc<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn cons(&self, elem: T) -> Self {
        List {
            head: Some(Rc::new(Node {
                elem,
                next: self.head.clone(),
            })),
        }
    }

    pub fn car(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn cdr(&self) -> Self {
        List {
            head: self.head.as_ref().and_then(|node| node.next.clone()),
        }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_deref(),
        }
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                head = node.next.take();
            } else {
                break;
            }
        }
    }
}

pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.map(|node| {
            self.next = node.next.as_deref();
            &node.elem
        })
    }
}

#[cfg(test)]
mod test {
    use super::List;

    macro_rules! im_list {
        () => {{
            let id = List::new();
            let v1 = id.cons(1).cons(2).cons(3);
            let v2 = v1.cdr().cons(4).cons(5).cons(6);
            let v3 = v2.cdr().cdr().cons(3);
            (id, v1, v2, v3)
        }};
    }

    macro_rules! verity {
        ( $id:expr,  $n:tt ) => {
            $id.iter().zip($n.into_iter()).map(|(&i, r)| {
                assert_eq!(i, r);
            });
        };
    }

    #[test]
    fn test() {
        let (id, v1, v2, v3) = im_list!();
        verity!(id, []);
        verity!(v1, [3, 2, 1]);
        verity!(v2, [6, 5, 4, 2, 1]);
        verity!(v3, [3, 4, 2, 1]);
    }
}
