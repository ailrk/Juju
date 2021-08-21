{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

-- Eh, this library allows you to make a email but it doesn't export the mail
-- type?

import           Control.Monad
import           Data.Data
import           Data.Default
import           Data.Foldable               (traverse_)
import           Data.Function
import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
-- import qualified Data.Text.Lazy.Builder      as T
import qualified Data.Text.Lazy.IO           as TL
import           Debug.Trace
import           GHC.Records

-- import qualified Data.Text                   as TL
import           GHC.Base                    (assert)
import           Network.HaskellNet.Auth
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Network.Mail.Mime

import           System.Environment
import           System.Exit
import           Text.Pretty.Simple

data Status = Unfilled | Filled
data Chunk = SomeText T.Text | Key T.Text deriving Show

chunkToText :: Chunk -> Maybe T.Text
chunkToText (SomeText n) = Just n
chunkToText (Key _)      = Nothing

type family MailThunkContent a where
  MailThunkContent Filled = TL.Text
  MailThunkContent Unfilled = ()

data MailThunk (status :: Status) =
  MailThunk { mtfrom    :: Address
            , mtto      :: Address
            , mtcc      :: Address
            , mtbcc     :: Address
            , mtsubject :: T.Text
            , mtcontent :: MailThunkContent status
            }

instance Default (MailThunk Unfilled) where
  def = MailThunk { mtfrom = "", mtto = "" , mtcc = ""
                  , mtbcc = "", mtsubject = "", mtcontent = () }

-- | parse one template file as list of chunks. key is trimed.
parseTemplate :: TL.Text -> Maybe [Chunk]
parseTemplate template
  | TL.null template = return []
  | otherwise = return $ template & TL.splitOn "{@" & foldr convert [] & fixLast
  where
    convert s acc
      | let (pre, rest) = TL.breakOnEnd "@}" s, not (TL.null rest) =
        let pre' = Key $ TL.toStrict (TL.strip (TL.take (TL.length pre - 2) pre))
            rest' = SomeText (TL.toStrict rest)
        in pre' : rest' : acc
      | otherwise = SomeText (TL.toStrict s) : acc
    fixLast xs = let pre = init xs
                     l = last xs
                  in case l of
                       SomeText t ->
                         if T.takeEnd 2 t == "@}"
                            then pre <> [(SomeText $ T.dropEnd 2 t)]
                            else xs
                       k ->  xs

-- >>> parseTemplate "asdjas {@ mm @} asd {@ as @}"
-- [Key "",SomeText "asdjas ",Key "mm",SomeText " asd ",SomeText " as "]

-- | parse one block
parseBlock :: T.Text -> Maybe (MailThunk Unfilled, [(T.Text, T.Text)])
parseBlock text = do
  guard (not . T.null $ text)
  let lines = fmap (T.breakOn ":") (T.lines text)
  return $ foldr convert (def,[]) lines
  where
    keywords = ["from", "to", "cc", "bcc", "subject"]
    convert s acc@(mt, holeMap)
      | let (key, value) = s, not . T.null . T.strip $ value =
        let updateMailThunk =
              let value' = T.strip . T.tail $ value
                  addr = Address Nothing  value' in
              case key of
                "from"    -> mt { mtfrom = addr }
                "to"      -> mt { mtto = addr }
                "cc"      -> mt { mtcc = addr }
                "bcc"     -> mt { mtbcc = addr }
                "subject" -> mt { mtsubject = value' }
                _         -> mt
            updateHoleMap = if not (key `elem` keywords)
                               then (key, T.strip . T.tail $ value) : holeMap
                               else holeMap
         in (updateMailThunk, updateHoleMap)
      | otherwise = acc

type HeaderAndHole = (MailThunk Unfilled, [(T.Text, T.Text)])

parseHeadersAndHoles :: T.Text -> Maybe [HeaderAndHole]
parseHeadersAndHoles text
  | T.null text = return $ []
  | otherwise =
    let ts = T.dropEnd 1 . fst . T.breakOnEnd "}" <$> (T.splitOn "{" text)
            & filter (not . T.null)
     in trace ("[ts]: " ++ show ts) $ traverse parseBlock ts

fillHoles :: [Chunk] -> [(T.Text, T.Text)] -> TL.Text
fillHoles chunks holeMap = foldr fill mempty chunks
  where
    fill (SomeText t) acc = TL.fromStrict t <> acc
    fill (Key k) acc =
      case lookup k holeMap of
        Just n  -> TL.fromStrict n <> acc
        Nothing -> acc

filledMt2Mail :: MailThunk Filled -> Mail
filledMt2Mail MailThunk{..} =
  simpleMail' mtto mtfrom mtsubject mtcontent

makeMail :: [Chunk] -> HeaderAndHole -> Mail
makeMail templateChunk hh = filledMt2Mail (makeMailThunk hh)
  where
    makeMailThunk :: HeaderAndHole -> MailThunk Filled
    makeMailThunk (MailThunk{..}, holeMap) =
      let filled = fillHoles templateChunk holeMap
       in trace ("[FILLED]: " ++ show filled) $ MailThunk {mtcontent = filled, ..}

sendIt :: FilePath -> FilePath -> FilePath -> Mail -> IO ()
sendIt host user pass mail = doSMTPSTARTTLS host $ \conn -> do
  authSuccess <- authenticate LOGIN user pass conn
  if authSuccess
     then sendMail mail conn
     else die "authentication failed"

  -- pPrint mail

run :: String -> String -> String -> String -> String -> IO ()
run template hhs host user pass = do
  template <- fromJust <$> do
    t <- TL.readFile template
    return $ parseTemplate t

  hhs <- fromJust <$> do
    i <- T.readFile hhs
    return $ parseHeadersAndHoles i

  let mails =  makeMail template <$> hhs
  traverse_ (sendIt host user pass) mails

main :: IO ()
main = do
  [template, hhs, host, user, pass] <- getArgs
  run template hhs host user pass
