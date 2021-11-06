# Loop

A loop dsl for monadic actions.

### Motivation

Looping in monadic actions always feel awkward. Hope this dsl provides a simple yet powerful enough solution for most use cases. This dsl is only aim for monadic actions.

The design is losely influenced by common lisp loop macro.


```haskell

main = do
  xs <- loopM (from 10 to 20 step 1 collecting) $ \i -> do
          putStrLn . show $ i
          i

  loopM (lock all) $ \i -> do
    ...

  let xs' = fmap Just xs
  loopM (while isJust across xs) $\i -> do
    putStrLn "hi I'm just"

  loopM (let yes =
         in while yes across xs) $ \(i, v) -> do
    ...

  loopM (fold xs from left) $ \(i, v) -> do
    ...

  loopM (fold xs from right)

  loopM (whle isJust across )
```
