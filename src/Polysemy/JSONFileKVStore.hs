{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Polysemy.JSONFileKVStore
  ( JSONParseException (..),
    runKVStoreAsJSONFileStore,
    eitherDecodeOrCreate,
  )
where

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics
import Path
import Polysemy
import Polysemy.Error
import Polysemy.KVStore
import qualified UnliftIO.Path.Directory as U

newtype JSONParseException = JSONParseException String
  deriving (Show, Eq, Generic)

instance Exception JSONParseException where
  displayException (JSONParseException x) = x

eitherDecodeOrCreate :: (ToJSON a, FromJSON a, MonadIO m) => Path b File -> a -> m (Either String a)
eitherDecodeOrCreate f x = do
  whenM (fmap not . U.doesFileExist $ f) $ liftIO $ encodeFile (toFilePath f) x
  liftIO $ eitherDecodeFileStrict' (toFilePath f)

-- | Run a `KVStore` as a json file stored as a `Map`.
--
-- @since 0.1.0.0
runKVStoreAsJSONFileStore ::
  ( Members '[Embed IO, Error JSONParseException] r,
    FromJSONKey k,
    ToJSONKey k,
    FromJSON v,
    ToJSON v,
    Ord k
  ) =>
  Path b File ->
  Sem (KVStore k v ': r) a ->
  Sem r a
runKVStoreAsJSONFileStore d = interpret \case
  LookupKV k -> do
    z <- eitherDecodeOrCreate d mempty
    case z of
      Left x -> throw @JSONParseException $ JSONParseException x
      Right x -> return $ Map.lookup k x
  UpdateKV k v -> do
    z <- eitherDecodeOrCreate d mempty
    case z of
      Left x -> throw $ JSONParseException x
      Right (x :: Map k v) -> embed $ encodeFile (toFilePath d) (Map.alter (const v) k x)
{-# INLINE runKVStoreAsJSONFileStore #-}
