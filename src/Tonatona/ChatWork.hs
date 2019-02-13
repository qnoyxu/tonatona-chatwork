module Tonatona.ChatWork
  ( run
  , Dsl
  , DslBackend
  , Config
  ) where

import Tonalude
import Tonatona (HasConfig(..), HasParser(..))
import TonaParser ((.||), argLong  , envVar, requiredVal)
import Tonatona.ChatWork.Internal
import Data.Text.Encoding (encodeUtf8)


-- | Main function.
run ::
     (HasConfig env Config)
  => Dsl env a
  -> RIO env a
run action = do
  Config {..} <- asks config
  runReaderT action (DslBackend . encodeUtf8 . untoken $ token)


------------
-- Config --
------------

data Config = Config
  { token :: Token
  }

newtype Token = Token
  { untoken :: Text
  }
  deriving (Show, Eq)

instance HasParser Token where
  parser = Token <$>
    requiredVal
      "Token for ChatWork API"
      ( argLong "token" .|| envVar "TOKEN")

instance HasParser Config where
  parser = Config
      <$> parser
