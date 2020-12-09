{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | This module contains helper functions to work with JSON
module TgramAPIJson
    (
      toJsonDrop,
      parseJsonDrop,
      OutboundMessage(..)
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T
import           GHC.Generics
import Data.Int (Int64)

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a.(Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a.(Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }

type ChatId = Int64

data OutboundMessage = OutboundMessage
    {
        msgChatId :: ChatId,
        msgTxt    :: T.Text
    }
    deriving (Eq, Show)

instance ToJSON OutboundMessage where
    toJSON (OutboundMessage msgChatId msgTxt) = object ["chat_id" .= msgChatId, "text" .= msgTxt]
