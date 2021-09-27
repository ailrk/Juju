{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}


module ClosureConversion () where

import           Control.Applicative          (liftA2)
import           Control.Monad.Trans.State    as State
import           Data.Char                    (isDigit, isLetter, isSpace)
import qualified Data.HashMap.Lazy            as HM
import qualified Data.HashSet                 as HS
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as S
import           Data.Typeable
import           Debug.Trace
import           Text.ParserCombinators.ReadP ((<++))
import qualified Text.ParserCombinators.ReadP as P
import           Text.Pretty.Simple           (pPrint, pShowNoColor, pString)

type Var = String

data Expr where
  Var :: Var -> Expr
  Apply :: { f :: Expr, args :: [Expr] } -> Expr
  Lambda :: { args ::  [Expr], body :: Expr } -> Expr

-- closure conversion only
  LambdaConverted :: { args :: [Expr] , body :: Expr } -> Expr
  MkClosure ::  { lam :: Expr, env :: Expr } -> Expr

  MkEnv :: { envlist :: [(Expr, Expr)] } -> Expr
  EnvRef :: { env :: Expr, var :: Expr } -> Expr
  ApplyClosure :: { f :: Expr, args :: [Expr] } -> Expr

instance Show Expr where
  show (Var v) = show v
  show (Lambda args body) =
    "(lambda (" <> mconcat (fmap show args) <> ")" <> show body <> ")"
  show (Apply f args) = "(" <> show f <> " " <> mconcat (fmap show args) <> ")"
  show (LambdaConverted args body) =
    "(lambda* (" <> mconcat (fmap show args) <> ")" <> show body <> ")"
  show (MkClosure lam env) = "(make-closure " <> show lam <> " " <> show env <> ")"
  show (MkEnv envlist) = "(make-env " <> mconcat (fmap showPair envlist) <> ")"
    where showPair = (\(v, e) -> "(" <> show v <> " " <> show e <> ")")
  show (EnvRef env var) = "(env-ref " <> show env <>  "" <> show var <>")"
  show (ApplyClosure f args) = "(apply-closure " <> show f <> " " <> mconcat (fmap show args) <> ")"

fromVar :: Expr -> Var
fromVar (Var n) = n
fromVar e       = error "expect a variable name, get " <> show e

-------------------------------------------------------------------------------
-- parser

identifierp = idp <* P.skipSpaces
  where
    idp = do
      first <- P.satisfy isLetter
      rest <- P.munch (\c -> isDigit c || isLetter c)
      return $ first : rest

-- >>> P.readP_to_S reservedp "lambda"

tokenp :: Char -> P.ReadP Char
tokenp c = P.char c <* P.skipSpaces

reserved :: String -> P.ReadP String
reserved s = P.string s <* P.skipSpaces

varp :: P.ReadP Expr
varp = Var <$> identifierp

-- (f as)
applyp :: P.ReadP Expr
applyp = do
  tokenp ('(')
  Apply <$> parse <*> P.manyTill parse (tokenp ')')

lambbdaBodyp :: ([Expr] -> Expr -> Expr) -> P.ReadP b -> P.ReadP Expr
lambbdaBodyp con prefix = prefix *> (tokenp '(') *> do
  ts <- P.manyTill identifierp (tokenp ')')
  expr <- parse
  return $ con (fmap Var ts) expr

lambdap :: P.ReadP Expr
lambdap = lambbdaBodyp Lambda (reserved "lambda")

lambdaConvertedp :: P.ReadP Expr
lambdaConvertedp = lambbdaBodyp LambdaConverted (reserved "lambda*")

mkClosurep :: P.ReadP Expr
mkClosurep = MkClosure <$> (reserved "mkclosure" *> parse) <*> parse

mkEnvp :: P.ReadP Expr
mkEnvp = MkEnv <$> (reserved "mkenv" *> pairsp)
  where
    pairp = (,) <$> (tokenp '(' *> varp) <*> (parse <* tokenp ')')
    pairsp = tokenp '(' *> (P.manyTill pairp (tokenp ')'))

envRefp :: P.ReadP Expr
envRefp = EnvRef <$> (reserved "envref" *> varp) <*> varp

applyClosurep :: P.ReadP Expr
applyClosurep = ApplyClosure
            <$> (reserved "apply-closure" *> parse)
            <*> (P.manyTill parse (tokenp ')'))

parse :: P.ReadP Expr
parse = parse1 <++ applyp
  where
    parse1 = lambdaConvertedp
         <++ lambdap
         <++ mkClosurep
         <++ mkEnvp
         <++ envRefp
         <++ varp
         <++ applyClosurep
         <++ P.between (tokenp '(') (tokenp ')') parse

-- >>> let c1 = "(mkclosure (lambda (env x) (envref env a)) (mkenv ((f f))))"
-- >>> readP_to_S parse c1
-- >>> readP_to_S parse "(lambda (x) (lambda (y) (x y)))"

type UniqueId = Int
type LC a = State UniqueId a

getUniqueId :: LC Int
getUniqueId = State.get >>= \i -> modify (\n -> n + 1) >> return i

free :: Expr  -> HS.HashSet Var
free (Lambda params body) =
  HS.difference (free body) (HS.fromList (fmap fromVar params))
free (LambdaConverted params body) =
  HS.difference (free body) (HS.fromList (fmap fromVar params))
free (Var v)             = HS.singleton v
free (MkClosure lam env) =  HS.union (free lam) (free env)
free (MkEnv env@(((v, e):_))) = HS.unions (fmap free (fmap snd env))
free (MkEnv []) = HS.empty
free (EnvRef env var) = free env
free (ApplyClosure f vs) = HS.unions (free f : fmap free vs)
free (Apply f vs) = HS.unions (free f : fmap free vs)

-- | substitute free variables with dictionary
substitue :: HM.HashMap Var (Expr ) -> Expr -> Expr
substitue dict expr@(Var v)
  | v `HM.member` dict = dict HM.! v
  | otherwise = expr
substitue dict (ApplyClosure lam params) =
  ApplyClosure (substitue dict lam) (fmap (substitue dict) params)
substitue dict l
  | (Lambda params body) <- l =
    let params' = HS.fromList (fmap fromVar params)
        dict' = HM.filterWithKey (\k _ -> k `HS.member` params') dict
     in Lambda params (substitue dict' body)
  | (LambdaConverted params body) <- l =
    let params' = HS.fromList (fmap fromVar params)
        dict' = HM.filterWithKey (\k _ -> k `HS.member` params') dict
     in LambdaConverted params (substitue dict' body)
  | (Apply lam params) <- l =
    Apply (substitue dict lam) (fmap (substitue dict) params)
  | (MkClosure lam env) <- l =
    MkClosure (substitue dict lam) (substitue dict env)
  | (MkEnv env) <- l= MkEnv (fmap (\(v,e) -> (v, substitue dict e)) env)
  | (EnvRef env ref) <- l = EnvRef (substitue dict env) ref

closureConvert :: Expr -> LC Expr
closureConvert expr = (show "closureConvert " <> show expr) `trace` convert expr
  where
    convert :: Expr  -> LC Expr
    convert expr@(Lambda params body) = do
      let fv = free expr
          env = fmap (\v -> (Var v, Var v)) (HS.toList fv)
      envSym <- genEnvSym
      sub <- do
          let mkref k = EnvRef (Var envSym) (Var k)
          return $ HM.mapWithKey (\k _ -> mkref k) (HS.toMap fv)
      let body' = substitue sub body
      params' <- return ((Var envSym) : params)
      return $ MkClosure (LambdaConverted params' body') (MkEnv env)
      where
        genEnvSym = fmap (\i -> "env" ++ show i) getUniqueId

    convert (Apply f args) = return $ ApplyClosure f args
    convert expr = return expr

-- (a -> m b)  (m b)

-- transform each node
transform :: (Expr -> LC Expr) ->  Expr -> LC Expr
transform t (Lambda params body) = (Lambda params) <$> (t body)
transform t (LambdaConverted params body) = (LambdaConverted params) <$> (t body)
transform t (MkClosure lam env) = MkClosure <$> (t lam) <*> (t env)
transform t expr@(Var _)         = return expr
transform t (MkEnv env) =
  MkEnv <$> (traverse (\(a, b) -> t b >>= \b' -> return (a, b')) env)
transform t (EnvRef env v) = EnvRef <$> (t env) <*> pure v
transform t (Apply f args) = Apply <$> (t f) <*> (traverse t args)
transform t (ApplyClosure f args) = ApplyClosure <$> (t f) <*> (traverse t args)


-- envs are linked
flatClosureConvert :: Expr -> Expr
flatClosureConvert expr = evalState (transformBottomUp closureConvert expr) 0
  where
    transformBottomUp f expr = f expr >>= transform t
      where t e = transformBottomUp f e

-- envs are copied
sharedClosureConvert :: Expr -> Expr
sharedClosureConvert expr = evalState (transformTopdown closureConvert expr) 0
  where
    transformTopdown f expr = transform t expr >>= f
      where t e = transformTopdown f e


example1 = "(lambda (g) (lambda (z) (lambda (x) (g x z a))))"

run1 :: IO ()
run1 = pPrint
     . fst
     . head
     $ P.readP_to_S (flatClosureConvert <$> parse) example1

run2 :: IO ()
run2 = pPrint
     . fst
     . head
     $ P.readP_to_S (sharedClosureConvert <$> parse) example1

-- readP_to_S (flatClosureConvert <$> parse) example1
