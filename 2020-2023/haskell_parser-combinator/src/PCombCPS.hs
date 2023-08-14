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
import           Control.Monad
import           Data.List.NonEmpty  as NonEmpty
import           GHC.Base            hiding (many)
import           GHC.Unicode         (isSpace)
import           Prelude             hiding (Parser, ReadS)



infixr 5 +++, <++

type ReadS a = String -> [(a, String)]

-------------------------------------------------------------------------------
-- P is a datatype that encodes operations of a parser.

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

instance Monad P where
  (Get f) >>= k           = Get $ f >=> k
  (Look f) >>= k          = Look $ f >=> k
  Fail >>= _              = Fail
  (Result x p) >>= k      = k x <|> (p >>= k)
  (Final (r :| rs)) >>= k = final [ys' | (x, s) <- (r:rs) , ys' <- run (k x) s]

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
  fail _ = R $ const Fail

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
pfail = R (const Fail)

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
    scan (c:cs) | p c = get >> scan cs >>= (const $ return (c:cs))
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
