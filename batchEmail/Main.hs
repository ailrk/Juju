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
import           Data.Foldable          (traverse_)
import           Data.Function
import           Data.Maybe             (fromJust)
import qualified Data.Text.Encoding     as T
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import           Debug.Trace
import           GHC.Records

import qualified Data.Text              as TL
import           GHC.Base               (assert)
import           Network.Mail.Mime      hiding (simpleMail)
import           Network.Mail.SMTP
import           System.Environment
import           System.Exit
import           Text.Pretty.Simple

data Status = Unfilled | Filled
data Chunk = SomeText T.Text | Key T.Text deriving Show

chunkToText :: Chunk -> Maybe T.Text
chunkToText (SomeText n) = Just n
chunkToText (Key _)      = Nothing

type family MailThunkContent a where
  MailThunkContent Filled = T.Text
  MailThunkContent Unfilled = ()

data MailThunk (status :: Status) =
  MailThunk { mtfrom    :: Address
            , mtto      :: [Address]
            , mtcc      :: [Address]
            , mtbcc     :: [Address]
            , mtsubject :: T.Text
            , mtcontent :: MailThunkContent status
            }

instance Default (MailThunk Unfilled) where
  def = MailThunk { mtfrom = "", mtto = [] , mtcc = []
                  , mtbcc = [], mtsubject = "", mtcontent = () }

-- | parse one template file as list of chunks. key is trimed.
parseTemplate :: T.Text -> Maybe [Chunk]
parseTemplate template
  | T.null template = return []
  | otherwise = return $ template & T.splitOn "{@" & foldr convert [] & fixLast
  where
    convert s acc
      | let (pre, rest) = T.breakOnEnd "@}" s, not (T.null rest) =
        let pre' = Key $ T.strip (T.take (T.length pre - 2) pre)
            rest' = SomeText rest
        in pre' : rest' : acc
      | otherwise = SomeText s : acc
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
              let value' = TL.strip . TL.tail . T.toStrict $ value
                  addr = Address Nothing  value' in
              case key of
                "from"    -> mt { mtfrom = addr }
                "to"      -> mt { mtto = addr : mtto mt }
                "cc"      -> mt { mtcc = addr : mtcc mt }
                "bcc"     -> mt { mtbcc = addr : mtbcc mt }
                "subject" -> mt { mtsubject = T.fromChunks [value'] }
                _         -> mt
            updateHoleMap = if not (key `elem` keywords)
                               then (key, T.tail value) : holeMap
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

fillHoles :: [Chunk] -> [(T.Text, T.Text)] -> T.Text
fillHoles chunks holeMap = trace ("[FILLHOLD]: " ++ show holeMap) $ T.toLazyText (foldr fill mempty chunks)
  where
    fill (SomeText t) acc = T.fromLazyText t <> acc
    fill (Key k) acc =
      case T.fromLazyText <$> lookup k holeMap of
        Just n  -> n <> acc
        Nothing -> acc

filledMt2Mail :: MailThunk Filled -> Mail
filledMt2Mail MailThunk{..} =
  simpleMail mtfrom mtto mtcc mtbcc (T.toStrict mtsubject) [plainPart mtcontent]

makeMail :: [Chunk] -> HeaderAndHole -> Mail
makeMail templateChunk hh = filledMt2Mail (makeMailThunk hh)
  where
    makeMailThunk :: HeaderAndHole -> MailThunk Filled
    makeMailThunk (MailThunk{..}, holeMap) =
      let filled = fillHoles templateChunk holeMap
       in trace ("[FILLED]: " ++ show filled) $ MailThunk {mtcontent = filled, ..}

sendIt :: FilePath -> FilePath -> FilePath -> Mail -> IO ()
sendIt host user pass mail = do
  pPrint mail
  sendMailWithLogin host user pass mail

main :: IO ()
main = do
  [template, infos, host, user, pass] <- getArgs

  template <- fromJust <$> do
    t <- T.readFile template
    return $ parseTemplate t

  hhs <- fromJust <$> do
    i <- T.readFile infos
    return $ parseHeadersAndHoles i

  let mails =  makeMail template <$> hhs
  -- traverse_ (\n -> putStrLn (show $ "[MAIL:]" <> show n <> show "\n") >> putStrLn "")  mails
  traverse_ (sendIt host user pass) mails
