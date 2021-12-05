module Actor where

-- Actor model
--  - each process has an id.
--  - sending and receiving messages are ansynchronous.
--  - each process holding it's own private state.
--
-- Pros:
--  - no manual multithreaded programming with locks because no shared memory.
--    Which means programm will be deadlock free.
--  - fault tolerant. A supervisor can reboot crashed actors.
--  - good for distributed programming.
--
-- Cons:
--  - Sometime shared memory is necessary.
--  - All communications are async means it's hard to maintain order.
--  - hard to debug, cause now you have several separated processes instead of
--    one.

