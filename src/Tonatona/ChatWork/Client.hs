module Tonatona.ChatWork.Client
  (
  -- Me
    getMe
  -- My
  , getMyStatus
  , getMyTasks
  -- Contacts
  , getContacts
  -- Rooms
  , getRooms
  , createRoom
  , getRoom
  , updateRoom
  , deleteRoom
  , leaveRoom
  , deleteRoom'
  , getMembers
  , updateMembersPermission
  , getMessages
  , postMessage
  , getMessage
  , getRoomTasks
  , createTask
  , getRoomTask
  , getFiles
  , getFile
  ) where

import Tonalude
import Tonatona.ChatWork.Internal
import qualified ChatWork as CW
import Data.Aeson (FromJSON(..))
import Data.Default.Class (def)
import Network.HTTP.Req (Req, responseBody, runReq)



-- Me

getMe :: Dsl env CW.Me
getMe =
  doClient CW.getMe


-- My

getMyStatus :: Dsl env CW.MyStatus
getMyStatus =
  doClient CW.getMyStatus


getMyTasks :: CW.GetMyTasksParams -> Dsl env CW.MyTasks
getMyTasks params =
  doClient $ (flip CW.getMyTasks) params


-- Contacts

getContacts :: Dsl env CW.Contacts
getContacts =
  doClient CW.getContacts


-- Rooms

getRooms :: Dsl env CW.Rooms
getRooms =
  doClient CW.getRooms


createRoom :: CW.CreateRoomParams -> Dsl env CW.RoomIdWrap
createRoom params =
  doClient $ (flip CW.createRoom) params


getRoom :: Int -> Dsl env CW.RoomDetail
getRoom rid =
  doClient $ (flip CW.getRoom) rid


updateRoom :: Int -> CW.UpdateRoomParams -> Dsl env CW.RoomIdWrap
updateRoom rid params =
  doClient $ (flip2 CW.updateRoom) rid params


deleteRoom :: Int -> Dsl env ()
deleteRoom rid =
  doClient $ (flip CW.deleteRoom) rid


leaveRoom :: Int -> Dsl env ()
leaveRoom rid =
  doClient $ (flip CW.leaveRoom) rid


deleteRoom' :: Int -> CW.DeleteRoomActionType -> Dsl env ()
deleteRoom' rid action =
  doClient $ (flip2 CW.deleteRoom') rid action


-- * Room Member

getMembers :: Int -> Dsl env CW.Members
getMembers rid =
  doClient $ (flip CW.getMembers) rid


updateMembersPermission :: Int -> CW.RoomMembersParams -> Dsl env CW.MembersPermission
updateMembersPermission rid params =
  doClient $ (flip2 CW.updateMembersPermission) rid params


-- * Room Message

getMessages :: Int -> Maybe CW.Force -> Dsl env CW.Messages
getMessages rid maybeForce =
  doClient $ (flip2 CW.getMessages) rid maybeForce


postMessage :: Int -> CW.MessageBody -> Dsl env CW.MessageIdWrap
postMessage rid body =
  doClient $ (flip2 CW.postMessage) rid body


getMessage :: Int -> Text -> Dsl env CW.Message
getMessage rid mid =
  doClient $ (flip2 CW.getMessage) rid mid


-- * Room Task

getRoomTasks :: Int -> CW.GetTasksParams -> Dsl env CW.RoomTasks
getRoomTasks rid params =
  doClient $ (flip2 CW.getRoomTasks) rid params


createTask :: Int -> CW.CreateTaskParams -> Dsl env CW.TaskIdsWrap
createTask rid params =
  doClient $ (flip2 CW.createTask) rid params


getRoomTask :: Int -> Int -> Dsl env CW.RoomTask
getRoomTask rid tid =
  doClient $ (flip2 CW.getRoomTask) rid tid


-- * Room File

getFiles :: Int -> Maybe CW.AccountId -> Dsl env CW.Files
getFiles rid aid =
  doClient $ (flip2 CW.getFiles) rid aid


getFile :: Int -> Int -> Maybe CW.CreateUrlFlag -> Dsl env CW.File
getFile rid fid maybeFlag =
  doClient $ (flip3 CW.getFile) rid fid maybeFlag





-- Lower level functions

doClient ::
     (FromJSON a)
  => (CW.ChatWorkClient -> Req (CW.ChatWorkResponse a))
  -> Dsl env a
doClient func = do
  t <- asks token
  let client = CW.ChatWorkClient t
  resp <- runReq def $ func client
  let body = responseBody resp
  case body of
    Left err -> throwM $ err
    Right me -> pure me


-- Helper functions

flip2 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
flip2 f = (\q r p -> f p q r)

flip3 :: (a -> b -> c -> d -> e) -> (b -> c -> d -> a -> e)
flip3 f = (\q r s p -> f p q r s)
