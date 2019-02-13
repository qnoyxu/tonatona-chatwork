module TonaApp.Main where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.ChatWork as TonaChatWork
import Tonatona.ChatWork.Client (createRoom, createTask, getContacts, getMe, postMessage)
import Network.HTTP.Req (HttpException)
import ChatWork (ChatWorkErrors(..), CreateRoomParams(..), CreateTaskParams(..), contactToAccountId, getRoomId, meToAccountId)



-- App


app :: RIO Config ()
app = do
  TonaLogger.logInfo $ display ("This is a sample project for tonatona-chatwork" :: Text)
  TonaChatWork.run doChatWorkClient
  `catches`
    [ Handler $ \(e :: ChatWorkErrors) ->
        chatWorkErrorHandler e
    , Handler $ \(e :: HttpException) ->
        httpErrorHandler e
    ]


doChatWorkClient :: TonaChatWork.Dsl Config ()
doChatWorkClient = do
  -- Get my account id
  myId <- meToAccountId <$> getMe
  -- Get account ids of my contacts
  _ids <- (fmap contactToAccountId) <$> getContacts
  -- Create room
  roomId <- getRoomId <$> createRoom (CreateRoomParams {
      cRoomDescription = Nothing
    , cIconPreset = Nothing
    , cMembersAdminIds = [myId]
    , cMembersMemberIds = Nothing
    , cMembersReadonlyIds = Nothing
    , cRoomName = "test"
    })
  -- Post message to the room
  _ <- postMessage roomId "Wellcome to Test Room !!"
  -- Create task
  _ <- createTask roomId (CreateTaskParams {
      getTaskBody = "Self-Introduction"
    , getTaskLimit = Nothing
    , getTaskToIds = [myId]
    })
  lift $ TonaLogger.logDebug $ display ("I Created Test Room and Tasks that Self-Introductions to my contact members." :: Text)


chatWorkErrorHandler :: ChatWorkErrors -> RIO Config ()
chatWorkErrorHandler err =
  TonaLogger.logError $ display $ "Encount error on chatwork-api: " <> tshow err

httpErrorHandler :: HttpException -> RIO Config ()
httpErrorHandler err =
  TonaLogger.logError $ display $ "Encount error on chatwork-api: " <> tshow err



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaChatWork :: TonaChatWork.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger

instance HasConfig Config TonaChatWork.Config where
  config = tonaChatWork


instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
      -- <*> parser
