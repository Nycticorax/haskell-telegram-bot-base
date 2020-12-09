{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text                   as T
import           Data.Version                (showVersion)
import           GHC.Generics                hiding (from)
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Req
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Paths_haskell_telegram_base as P
import           Servant
import           System.Environment
import           TgramAPIJson
import           TgramAPITypes

newtype Version = Version { version :: T.Text } deriving (Show, Generic)


instance ToJSON Version


type BotAPI = "version" :> Get '[JSON] Version
         :<|> "webhook"
              :> Capture "secret" T.Text
              :> ReqBody '[JSON] Update
              :> Post '[JSON] ()


botApi :: Proxy BotAPI
botApi = Proxy


startApp :: IO ()
startApp = do
    env <- getEnvironment
    manager' <- newManager tlsManagerSettings
    let telegramToken' = fromJust $ lookup "TELEGRAM_TOKEN" env
        paymentsToken' = fromJust $ lookup "PAYMENTS_TOKEN" env
        port = read $ fromJust $ lookup "PORT" env
        config = BotConfig
            { telegramToken = T.pack $ "bot" <> telegramToken'
            , paymentsToken = T.pack paymentsToken'
            , manager = manager'
            }
    run port $ app config


newtype Bot a = Bot { runBot :: ReaderT BotConfig Handler a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader BotConfig, MonadError ServerError)


data BotConfig = BotConfig {
    telegramToken :: T.Text,
    paymentsToken :: T.Text,
    manager       :: Manager
}


app :: BotConfig -> Application
app config = serve botApi $ initBotServer config


initBotServer :: BotConfig -> Server BotAPI
initBotServer config = hoistServer botApi (flip runReaderT config . runBot) botServer


botServer :: ServerT BotAPI Bot
botServer = returnVersion :<|> handleWebhook
    where version' = Version $ T.pack $ showVersion P.version
          returnVersion :: Bot Version
          returnVersion = return version'
          handleWebhook :: T.Text -> Update -> Bot ()
          handleWebhook secret update = do
                token <- asks telegramToken
                if EQ == compare secret token then
                    case message update of
                        Just m  -> echo m token
                        Nothing -> throwError err400
                else throwError err403

echo m t =
  let   c = chat_id . chat $ m
        message_content = fromMaybe "Empty text mesage" $ text m
  in    void . reqSend t "sendMessage" $ OutboundMessage c message_content


reqSend :: (MonadIO m) => T.Text -> T.Text -> OutboundMessage -> m IgnoreResponse
{- Performs request for posting outbound message -}
reqSend token postMethodName encodedMsg = runReq defaultHttpConfig $ do
    let url = https "api.telegram.org" /: token
        reqUrl = url /: postMethodName
    req Network.HTTP.Req.POST reqUrl (ReqBodyJson encodedMsg) ignoreResponse mempty
