

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- Of course the entire mtl is based on newtype deriving
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Cat.Cat.MyMTL where
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Data.Functor.Classes       (Eq1 (..), Ord1 (..), Read1 (..),
                                             Show1 (..), compare1, eq1,
                                             readsData, readsPrec1,
                                             readsUnaryWith, showsPrec1,
                                             showsUnaryWith)
import           Data.Functor.Contravariant
import           Data.Functor.Identity

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.List

import           Prelude                    hiding (readFile, writeFile)
import qualified Prelude                    (readFile, writeFile)

{- The idea is simply adding constraint to a polymorphic type enabling more
   functions it can use, we thus get the extensibility.
-}

-------------------------------------------------------------------------------
-- define MTL style class constraints
-------------------------------------------------------------------------------

-- define a IdentityT wrapper, this is our algebra type.
newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Eq1 f => Eq1 (IdentityT f) where
  liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y
  {-# INLINE liftEq #-}

instance Ord1 f => Ord1 (IdentityT f) where
  liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y
  {-# INLINE liftCompare #-}

instance Read1 f => Read1 (IdentityT f) where
  liftReadsPrec rp r1 = readsData $
    readsUnaryWith (liftReadsPrec rp r1) "IdentityT" IdentityT

instance Show1 f => Show1 (IdentityT f) where
  liftShowsPrec sp s1 d (IdentityT m) =
    showsUnaryWith (liftShowsPrec sp s1) "IdentityT" d m

-- use Eq1, Ord1... etc to implement Eq, Ord... directly
instance (Eq1 f, Eq a) => Eq (IdentityT f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where compare = compare1
instance (Read1 f, Read a) => Read (IdentityT f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (IdentityT f a) where showsPrec = showsPrec1

instance (Functor m) => Functor (IdentityT m) where
  fmap f = mapIdentityT (fmap f)
  {-# INLINE fmap #-}

instance (Foldable f) => Foldable (IdentityT f) where
  foldMap f (IdentityT t) = foldMap f t
  {-# INLINE foldMap #-}

  foldr f z (IdentityT t) = foldr f z t
  {-# INLINE foldr #-}

  foldl f z (IdentityT t) = foldl f z t
  {-# INLINE foldl #-}

  foldr1 f (IdentityT t) = foldr1 f t
  {-# INLINE foldr1 #-}

  foldl1 f (IdentityT t) = foldl1 f t
  {-# INLINE foldl1 #-}

-- | lift an unary operator to the new monad.
mapIdentityT :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f = IdentityT . f . runIdentityT
{-# INLINE mapIdentityT #-}

instance Traversable f => Traversable (IdentityT f) where
  traverse f (IdentityT a) = IdentityT <$> traverse f a
  {-# INLINE traverse #-}

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (<*>) = undefined


-------------------------------------------------------------------------------
-- we can provide default implementation
-------------------------------------------------------------------------------

class Monad m => MonadFileSystem m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

  default readFile :: (MonadTrans t, MonadFileSystem m', m ~ t m')
                   => FilePath
                   -> m String
  readFile a = lift $ readFile a

  default writeFile :: (MonadTrans t, MonadFileSystem m', m ~ t m')
                    => FilePath
                    -> String
                    -> m ()
  writeFile a b = lift $ writeFile a b

-- we need to provide an instance with every other monad transformers,
instance MonadFileSystem m => MonadFileSystem (ExceptT e m)
instance MonadFileSystem m => MonadFileSystem (MaybeT m)
instance MonadFileSystem m => MonadFileSystem (ReaderT r m)
instance MonadFileSystem m => MonadFileSystem (StateT s m)
instance (MonadFileSystem m, Monoid w) => MonadFileSystem (WriterT w m)

instance MonadFileSystem IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

newtype InMemoryFileSystemT m a = InMemoryFileSystemT (StateT [(FilePath, String)] m a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader r, MonadWriter w)

instance Monad m => MonadFileSystem (InMemoryFileSystemT m) where
  readFile path = InMemoryFileSystemT $ do
    vfs <- get
    case lookup path vfs of
      Just contents -> pure contents
      Nothing       -> error ("readFile: no such fille" ++ path)

  writeFile path contents = InMemoryFileSystemT $ modify $ \vfs ->
    (path, contents) : delete (path, contents) vfs

