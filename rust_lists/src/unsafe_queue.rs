use std::ptr;

pub struct List<T> {
    head: Link<T>,
    tail: *mut Node<T>,
}

type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List {
            head: None,
            tail: ptr::null_mut(),
        }
    }

    pub fn push(&mut self, elem: T) {
        let mut new_tail = Box::new(Node { elem, next: None });
        let raw_tail: *mut _ = &mut *new_tail;
        if !self.tail.is_null() {
            unsafe {
                (*self.tail).next = Some(new_tail);
            }
        } else {
            self.head = Some(new_tail);
        }
        self.tail = raw_tail;
    }

    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|head| {
            let head = *head;
            self.head = head.next;

            if let None = self.head {
                self.tail = ptr::null_mut();
            }
            head.elem
        })
    }

    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.elem)
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.elem)
    }

    // a -> b -> c
    pub fn remove(&mut self, i: usize) {
        let mut count = i;
        while let Some(boxed_node) = &mut self.head {
            if i == 1 {
                boxed_node.next.take().map(|mut node1| {
                    node1.next.take().map(|mut node2| {
                        node1.next.take();
                        boxed_node.next = Some(node2);
                    });
                });
            }
        }
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

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut current = self.head.take();
        while let Some(mut boxed_node) = current {
            current = boxed_node.next.take();
        }
    }
}

pub struct IntoIter<T>(List<T>);

pub struct Iter<'a, T> {
    next: Option<&'a Node<T>>,
}

pub struct IterMut<'a, T> {
    next: Option<&'a mut Node<T>>,
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
