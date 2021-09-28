-- A ReadP style parser combinator

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- TODO @unfinished
module PCombCPS where
-------------------------------------------------------------------------------
-- inefficient ReadS implementation

import           Control.Applicative
import           Control.Monad       (ap)
import           Data.List.NonEmpty  as NonEmpty
import           GHC.Base            hiding (many)
import           GHC.Unicode         (isSpace)
import           Prelude             hiding (Parser, ReadS)

infixr 5 +++, <++

type ReadS a = String -> [(a, String)]

-------------------------------------------------------------------------------
-- P is a datatype that encodes operations of a parser.
-- What can P do?
-- P can:
--  - consume char from the input stream
--  - lookup in the input stream
--  - return a result on success
--  - return fail on faliure
--  - return a final state of the parser and finish parsing.
-- We encode these notions into a data type.
-- It's easy to encode data. E.g To indicate the parser is failed we only need
-- an empty data constructor Fail; To indicate the parser is finished we need
-- Final constructor with the current state of everything.
--
-- But get and lookup are behaviors, how do we encode those into a datatype? The
-- only thing we can do is to embed functions into each data constructor.

-- Get :: (a -> P a)
data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a, String))
  deriving Functor

instance MonadFail P where
  fail _ = Fail

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

-- P is a monad
-- It's clear that you can have list as a monad, or maybe be a monad.
-- But what does it mean to have P to be a monad? It's a sum type with  5
-- branches...
--
-- Well, you can compose these data constructors with each other. The type just
-- indicates we are working with these 5 consturcors, it doesn't convey other
-- information. For each constructor, we can define it's special
-- bind implementation: how does it interact with other constructor.
--
-- k : a -> P a: is a polymorphic function over P, so it works on all possible
-- constructor of P. Our monad defines given a constructor of P, a monadic
-- function k, what to do next.
--
-- In this sense, k can be though as continuation, and The monad is an
-- interpreter defines a small language that is described in haskell algebraic
-- data type.
--
-- So essentially we defined a dsl for a parser, and use monad to conpose
-- elements in the dsl. The dsl is static.
instance Monad P where
  (Get f) >>= k           = Get $ \c -> f c >>= k
  (Look f) >>= k          = Look $ \s -> f s >>= k
  Fail >>= _              = Fail
  (Result x p) >>= k      = k x <|> (p >>= k)
  (Final (r :| rs)) >>= k = final [ys' | (x, s) <- (r:rs) , ys' <- run (k x) s]

-- Define what happens when we have two operations side by side.
-- another way to compose.
-- Alternative doesn't care what's inside the applicative. It just combines two
-- applicatives like monoid.
instance Alternative P where
  empty = Fail
  Get f1 <|> Get f2          = Get $ \c -> f1 c <|> f2 c  -- combine two Gets

  Result x p <|> q           = Result x (p <|> q)     -- results are delivered asap
  p <|> Result x q           = Result x (p <|> q)

  Fail <|> p                 = p
  p <|> Fail                 = p

  Final r <|> Final t        = Final (r <> t)
  Final (r :| rs) <|> Look f = Look $ \s -> Final (r :| (rs ++ run (f s) s))
  Final (r :| rs) <|> p = Look $ \s -> Final (r :| (rs ++ run p s))
  Look f <|> Final r = Look $ \s -> Final (case run (f s) s of
                                             []     -> r
                                             (x:xs) -> (x :| xs) <> r)

  Look f <|> Look g = Look $ \s -> f s <|> g s
  Look f <|> p = Look $ \s -> f s <|> p
  p <|> Look f = Look $ \s -> p <|> f s

-- Now our P is a Monad, Applicative, and Alternative. So how does it evaluates?
-- 1. the dsl program consists of sequence of monadic functions k :: (a -> P b).
-- Each time we get a new P a, we need it to k and get a P b, and so on.
-- (a -> P b) give us the opportunity ot work with element within the monad.
--
-- Note monadic function means it takes a values, do something, return a new
-- value with some effects. This is essentially what imperative langauges does,
-- in fact, monad is one of the best way to get denotional semantic of
-- imperative programs.
--
-- 2. when we have two P a, we can combine them to get a single P a. Different
-- from monoid, alternatives carries effect, so combine two P a means also
-- perform the effect .
--
-- e.g
--
-- let x = (P a)
--     >>= (\a -> ... (P b))
--     >>= (\b -> ... (P c))
-- let y = (P a')
--     >>= (\a -> ... (P b'))
--     >>= (\b -> ... (P c'))
-- let z = x <|> y
--
-- this is the basic flow of the interpreter.

-- only return final if the list is non empty
final :: [(a, String)] -> P a
final []     = Fail
final (r:rs) = Final (r :| rs)

-- run a (P a) parser.
run :: P a -> ReadS a
run (Get f) (c:cs) = run (f c) cs
run (Look f) s     = run (f s) s
run (Result x p) s = (x, s) : run p s     -- run result appends result
run (Final rs) _   = NonEmpty.toList rs   -- parser end
run _ _            = []

-- There are really two components for Parser. We want to compose effectful
-- computation, so it's a monad. We want to combine two P, so it's alternative.
--
-- If we think typeclass as function from type to terms, we can appreciate the
-- abstraction better:
--    Type P is monad and applicative. -> It can compose with effectful
--    operations; two value of type P can be combinesd.
-- The implementation is something we worry later on.

-- Note there are not only one datatype that works like P. Many others also
-- works in a similar way:
--
--  1. a type T
--  2. T is a monad
--  3. T is an alternative
--
-- In the sense maybe works the same way as P, list works the same way as P,
-- State works the same way as P, etc...
--
-- They're different in the sense of their specific implementations, and that's
-- what gives a datatype concrete semantics.

-------------------------------------------------------------------------------
-- Parser
-- If an action takes parameters, it's (a -> P b)
-- if it doesn't it's (\_ -> P b). Either way is captured. by the type.
--
-- Note here although (a -> P b) -> P b is a function, we can't just use
-- functions instance because it has different semantics.

newtype Parser a = R (forall b. (a -> P b) -> P b)

instance Functor Parser where
  fmap h (R f) = R $ \k -> f (k . h)

instance Applicative Parser where
  pure x = R (\k -> k x)
  (<*>) = ap

instance Monad Parser where
  R m >>= f = R $ \k -> m $ \a -> let R m' = f a in m' k

instance MonadFail Parser where
  fail _ = R $ \_ -> Fail

instance Alternative Parser where
  empty = pfail
  (<|>) = (+++)

-------------------------------------------------------------------------------
-- Parser Operations

-- consume and return the next character Failes if there is no input left.
get :: Parser Char
get = R Get

-- look ahead wihtout consuming
look :: Parser String
look = R Look

pfail :: Parser a
pfail = R (\_ -> Fail)

-- choice
(+++) :: Parser a -> Parser a -> Parser a
R f1 +++ R f2 = R $ \k -> f1 k <|> f2 k

-- left baised choice. the right parser is discard. as long as left parser
-- produce any result
(<++) :: Parser a -> Parser a -> Parser a
R f' <++ q = do
  s <- look
  probe (f' return) s 0#
  where
    probe (Get f) (c:cs) n   = probe (f c) cs (n +# 1#)
    probe (Look f) s n       = probe (f s) s n
    probe p@(Result _ _) _ n = discard n >> R (p >>=)
    probe (Final r) _ _      = R (Final r >>=)
    probe  _ _ _             = q    -- if all others failes

    discard 0# = return ()
    discard n  = get >> discard (n -# 1#)

-- take a parser, return a parser that does the samething but also character
-- read.
gather :: Parser a -> Parser (String, a)
gather (R m) = R $ \k -> gath id (m (\a -> return (\s -> k (s, a))))
  where
    gath :: (String -> String) -> P (String -> P b) -> P b
    gath l (Get f)      = Get $ \c -> gath (l.(c:)) (f c)
    gath _ Fail         = Fail
    gath l (Look f)     = Look $ \s -> gath l (f s)
    gath l (Result k p) = k (l mempty) <|> gath l p
    gath _ (Final _)    = errorWithoutStackTrace "no readS_to_P"


-------------------------------------------------------------------------------
-- combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = get >>= (\c -> if p c then return c else pfail)

char :: Char -> Parser Char
char c = satisfy (== c)

eof :: Parser ()
eof = look >>= \s -> if null s then pfail else return ()

string :: String -> Parser String
string this = do
  s <- look
  scan this s
  where
    scan [] _ = return this
    scan (x:xs) (y:ys) | x == y = get >> scan xs ys
    scan _ _ = pfail

-- parser the first zero ore more characters satisfying the predicate
munch :: (Char -> Bool) -> Parser String
munch p = look >>= scan
  where
    scan (c:cs) | p c = get >> scan cs >>= \s -> return (c:cs)
    scan _ = return ""

munch1 :: (Char -> Bool) -> Parser String
munch1 p = undefined

choice :: [Parser a] -> Parser a
choice = undefined

skipSpaces :: Parser ()
skipSpaces = undefined

count :: Int -> Parser a -> Parser [a]
count = undefined

between :: Parser open -> Parser close -> Parser a -> Parser a
between = undefined

option :: a -> Parser a -> Parser a
option = undefined

optional :: Parser a -> Parser ()
optional = undefined

many :: Parser a -> Parser [a]
many = undefined

many1 :: Parser a -> Parser [a]
many1 = undefined

skipMany :: Parser a -> Parser ()
skipMany = undefined

skipMany1 :: Parser a -> Parser ()
skipMany1 = undefined

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy = undefined

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 = undefined

endBy :: Parser a -> Parser sep -> Parser [a]
endBy = undefined

endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 = undefined

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr = undefined

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl = undefined

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 = undefined

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 = undefined

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = undefined


-------------------------------------------------------------------------------
-- conversion

readPtoS :: Parser a -> ReadS a
readPtoS (R f) = run (f return)

readStoP :: ReadS a -> Parser a
readStoP r = R $ \k -> Look (\s -> final [bs'' | (a, s') <- r s, bs'' <- run (k a) s'])
