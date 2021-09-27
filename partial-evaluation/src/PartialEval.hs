module Staging.PartialEval where

-- https://www.researchgate.net/publication/213882771_Partial_Evaluation_and_Automatic_Program_Generation

-- basically currying for the entire program
-- if you partial apply some argumnet to your program, you can generate a
-- partially evaluated new program which runs faster because it alreay done
-- part of the work.

--
