module CSP where

-- Communication sequential process
--   - each process is anomyous
--   - send and receive are paired and synchronized.
--   - one channel can be shared by multiple processes.
--   - M:N model runs green threads on top of system threads.
--


-- Pros:
--  - there is a maxiumum channel buffer size, so don't need to worry about
--    infinite mailbox with messages.
--  - avoid coupling between producer and consumer.
--  - messages are delivered in order
--
-- Cons:
--  - Single machine solution, doesn't scale in a distributed system.


