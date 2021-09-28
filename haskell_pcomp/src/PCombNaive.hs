{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PCombNaive where

-- a naive parser combinator implementation.


import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Data.Char
import           Data.String
import qualified Data.Text              as T
import           Prelude

type Input = T.Text

-- put error types to another type
-- this simplify pattern matching.
data PError
  = ErrorEOF
  | EOF Input
  | UnknownChar Char
  | UnknownString T.Text

data PResult a
  = PError PError
  | Result Input a

instance Show a => Show (PResult a) where
  show (Result i a) = "Result: " ++ show i ++ " " ++ (show a)
  show (PError err) = case err of
    (EOF i)           -> "Expected EOF, but got" ++ (show i)
    (UnknownChar c)   -> "Unknown char" ++ (show c)
    (UnknownString t) -> "UnknownString" ++ (show t)
    ErrorEOF          -> "Parsing aborted because of unexpected EOF"

instance Functor PResult where
  fmap f (Result i a) = Result i $ f a
  fmap _ (PError err) = PError err

newtype ParserT m a = ParserT {parseT :: Input -> m (PResult a)}

type Parser a = ParserT Identity a

instance (Monoid a, Monad m) => Semigroup (ParserT m a) where
  pa <> pb = do
    pa' <- pa
    pb' <- pb
    return $ pa' <> pb'

instance (Monoid a, Monad m) => Monoid (ParserT m a) where
  mempty = ParserT $ \_ -> return $ PError ErrorEOF

instance Monad m => Functor (ParserT m) where
  fmap f p = ParserT $ \s -> do
    a <- parseT p s
    return $ f <$> a

instance Monad m => Applicative (ParserT m) where
  pure a = ParserT $ \s -> return $ Result s a
  ParserT f <*> ParserT x = ParserT $ \s ->
    f s >>= \case
      Result s' g -> (fmap . fmap) g (x s')
      PError err  -> return $ PError err

instance Monad m => Alternative (ParserT m) where
  empty = ParserT $ \_ -> return $ PError ErrorEOF
  (<|>) = option

instance Monad m => Monad (ParserT m) where
  return = pure
  ParserT m >>= f = ParserT $ \s -> do
    m s >>= \case
      Result s' a -> parseT (f a) s'
      PError err  -> return $ PError err

instance MonadTrans ParserT where
  lift m = ParserT $ \s -> do
    c <- m
    return $ Result s c

-- always fail on c
errorChar :: (Monad m) => Char -> ParserT m a
errorChar c = ParserT $ \_ -> return . PError $ UnknownChar c

--0 always return the same result.
constant :: (Monad m) => PResult a -> ParserT m a
constant = ParserT . const . return

-- success as long as input is not empty.
item :: (Monad m) => ParserT m Char
item = ParserT $ \xs ->
  return $
    case T.length xs of
      0 -> PError ErrorEOF
      _ -> (Result (T.tail xs) (T.head xs))

value :: (Monad m) => a -> ParserT m a
value a = ParserT $ \xs -> return $ Result xs a

satisfy :: (Monad m) => (Char -> Bool) -> ParserT m Char
satisfy pred = do
  a <- item
  if (pred a) then return a else errorChar a

char :: (Monad m) => Char -> ParserT m Char
char c = satisfy (== c)

option :: Monad m => ParserT m a -> ParserT m a -> ParserT m a
ParserT x `option` ParserT y = ParserT $ \s ->
  x s >>= \case
    t@(Result _ _) -> return t
    _              -> y s

digit :: (Monad m) => ParserT m Char
digit = satisfy isDigit

space :: (Monad m) => ParserT m Char
space = satisfy isSpace

-- cons two parser's result together
(<:>) :: (Monad m) => ParserT m a -> ParserT m [a] -> ParserT m [a]
pa <:> pxs = do
  a <- pa
  xs <- pxs
  return $ [a] ++ xs

oneOf :: (Monad m) => [Char] -> ParserT m Char
oneOf xs = satisfy (flip elem xs)

spaces1 :: (Monad m) => ParserT m String
spaces1 = some space

spaces :: (Monad m) => ParserT m String
spaces = many space

lower :: (Monad m) => ParserT m Char
lower = do
  a <- (satisfy isLetter)
  return $ toLower a

upper :: (Monad m) => ParserT m Char
upper = do
  a <- (satisfy isLetter)
  return $ toUpper a

alpha :: (Monad m) => ParserT m Char
alpha = satisfy isAlpha

thisMany :: (Monad m) => Int -> ParserT m a -> ParserT m [a]
thisMany n = (fmap $ take n) . many

natural :: (Monad m) => ParserT m Integer
natural = digit >>= \x -> return $ read [x]

string :: (Monad m) => String -> ParserT m String
string []         = return []
string t@(x : xs) = char x *> string xs *> return t

token :: (Monad m) => ParserT m a -> ParserT m a
token p = p >>= \a -> spaces >> return a

reserved :: (Monad m) => String -> ParserT m String
reserved s = token $ string s

numer :: (Monad m) => ParserT m Int
numer = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read $ s ++ cs

float :: (Monad m) => ParserT m Double
float = do
  s <- string "-" <|> return []
  cs <- some digit
  dot <- string "." <|> return []
  crs <- some digit
  return $ read $ s ++ cs ++ dot ++ crs
