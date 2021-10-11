pub struct List<T> {
    head: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn push(&mut self, elem: T) {
        let new_node = Box::new(Node {
            elem,
            next: self.head.take(),
        });
        self.head = Some(new_node);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|node| {
            self.head = node.next;
            node.elem
        })
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.elem)
    }

    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            next: self.head.as_deref(),
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            next: self.head.as_deref_mut(),
        }
    }
}

// avoid non tail recursive drop.
impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut current = self.head.take();
        while let Some(mut boxed_node) = current {
            current = boxed_node.next;
        }
    }
}

pub struct IntoIter<T>(List<T>);
impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop()
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

pub struct IterMut<'a, T> {
    next: Option<&'a mut Node<T>>,
}
impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|node| {
            self.next = node.next.as_deref_mut();
            &mut node.elem
        })
    }
}

#[cfg(test)]
mod test {
    use super::List;

    macro_rules! simple_list {
        () => {{
            let mut tmp = List::new();
            tmp.push(1);
            tmp.push(2);
            tmp
        }};
    }

    macro_rules! verfiy {
        ( $iter:expr, $n:tt ) => {
            $iter.zip($n.into_iter()).map(|(&l, r)| {
                assert_eq!(l, r);
            });
        };
    }

    #[test]
    fn basic() {
        let mut list = simple_list!();
        assert_eq!(list.pop().unwrap(), 2);
        assert_eq!(list.pop().unwrap(), 1);
        assert_eq!(list.pop(), None);
        list = simple_list!();
        assert_eq!(*list.peek().unwrap(), 2);
        assert_eq!(*list.peek().unwrap(), 2);
        *list.peek_mut().unwrap() = 3;
        assert_eq!(*list.peek().unwrap(), 3);
    }

    #[test]
    fn iters() {
        {
            let mut list = simple_list!();
            let iter = list.iter();
            verfiy!(iter, [2, 1]);
        };

        {
            let mut list = simple_list!();
            let mut iter = list.iter_mut();
            for n in iter {
                *n = 0;
            }
            verfiy!(list.iter(), [0, 0]);
        };

        {
            let mut list = simple_list!();
        };
    }
}
