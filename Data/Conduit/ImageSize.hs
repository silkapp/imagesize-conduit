{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Detect the image size without opening the image itself.
module Data.Conduit.ImageSize
  ( Size (..)
  , FileFormat (..)
  , sinkImageInfo
  , sinkImageSize
  ) where

import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as S
import Data.ByteString.Char8 ()
import Data.ByteString.Lazy.Char8 ()
import qualified Data.Typeable as T
import Control.Applicative ((<$>), (<*>))


data Size = Size { width :: Int, height :: Int }
  deriving (Show, Eq, Ord, Read, T.Typeable)


data FileFormat = GIF | PNG | JPG
  deriving (Show, Eq, Ord, Read, Enum, T.Typeable)


-- | Specialized version of 'sinkImageInfo' that returns only the
-- image size.
sinkImageSize :: Monad m => Consumer S.ByteString m (Maybe Size)
sinkImageSize = fmap (fmap fst) sinkImageInfo


-- | Find out the size of an image.  Also returns the file format
-- that parsed correctly.  Note that this function does not
-- verify that the file is indeed in the format that it returns,
-- since it looks only at a small part of the header.
sinkImageInfo :: Monad m => Consumer S.ByteString m (Maybe (Size, FileFormat))
sinkImageInfo = start id
  where
    start front = await >>= maybe (return Nothing) (pushHeader front)

    pushHeader front bs'
      | S.length bs >= 11 && S.take 3 bs == S.pack [0xFF, 0xD8, 0xFF] =
        leftover (S.drop 4 bs) >> jpg
      | S.length bs >= 6 && S.take 6 bs `elem` gifs =
        leftover (S.drop 6 bs) >> gif
      | S.length bs >= 8 && S.take 8 bs == S.pack [137, 80, 78, 71, 13, 10, 26, 10] =
        leftover (S.drop 8 bs) >> png
      | S.length bs < 11 = start $ S.append bs
      | otherwise = leftover bs >> return Nothing
      where
      bs = front bs'

    gifs = ["GIF87a", "GIF89a"]
    gif = do
      b <- CB.take 4
      let go x y = fromIntegral x + (fromIntegral y) * 256
      return $ case L.unpack b of
        [w1, w2, h1, h2] -> Just (Size (go w1 w2) (go h1 h2), GIF)
        _ -> Nothing

    png = do
      CB.drop 4
      hdr <- CB.take 4
      if hdr == "IHDR"
        then do
          mw <- getInt 4 0
          mh <- getInt 4 0
          return $ (\w h -> (Size w h, PNG)) <$> mw <*> mh
        else return Nothing

    jpg = do
      mi <- getInt 2 0
      case mi of
        Nothing -> return (Just (Size 1 1, JPG))
        Just i -> do
          CB.drop $ i - 2
          jpgFrame

    jpgSofMarkerBytes = [0xC0, 0xC1, 0xC2, 0xC3, 0xC5, 0xC6, 0xC7,
                         0xC9, 0xCA, 0xCB, 0xCD, 0xCE, 0xCF]

    jpgFrame = do
      mx <- CB.head
      case mx of
        Just 0xFF -> do
          my <- CB.head
          case my of
            Just b | b `elem` jpgSofMarkerBytes -> do
              _  <- CB.take 3
              mh <- getInt 2 0
              mw <- getInt 2 0
              return $ (\w h -> (Size w h, JPG)) <$> mw <*> mh
            Just _ -> jpg
            Nothing -> return Nothing
        _ -> return Nothing

getInt :: (Monad m, Integral i)
     => Int
     -> i
     -> Consumer S.ByteString m (Maybe i)
getInt 0 i = return $ Just i
getInt len i = do
  mx <- CB.head
  case mx of
    Nothing -> return Nothing
    Just x -> getInt (len - 1) (i * 256 + fromIntegral x)
