module STM where

-- shared memory approach with transaction.

-- Pros:
--  - Much simpler then locked based shared memory model.
--  - Avoid deadlock
--  - Don't need to restructure the code.
--
-- Cons:
--  - IO operations are hard to rollback.
