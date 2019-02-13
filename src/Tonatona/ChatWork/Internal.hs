module Tonatona.ChatWork.Internal
  ( Dsl
  , DslBackend(..)
  ) where

import Tonalude
import ChatWork.Client (Token)

type Dsl env
  = ReaderT DslBackend (RIO env)

data DslBackend = DslBackend
  { token :: Token
  }
  deriving (Show, Eq)
