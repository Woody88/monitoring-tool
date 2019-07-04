{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module API
    ( startApp
    , app
    , createUserChannel
    ) where

import           Data.Aeson
import           Data.Aeson.Text
import           Data.Aeson.TH
import           Data.Binary.Builder         as BB
import           Data.Text.Encoding          as TE
import           Data.Text.Lazy              as TL
import           EventManager                as EM
import           Network.HTTP.Types.Status   (status404)
import           Network.Wai
import           Network.Wai.EventSource     (ServerEvent,
                                              ServerEvent (ServerEvent),
                                              eventData, eventId, eventName,
                                              eventSourceAppChan)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           RIO                         hiding (Handler)
import           Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API =  "users" :> Get '[JSON] [User]               -- GET /users it return a json array of user
       :<|> "users" :> PostNoContent '[JSON] NoContent  -- POST /users it does not take any request body and it does not return anything
       :<|> "channel" :> "users" :> Raw                 -- GET /channel/users the server sent event stream it keeps the connection open

startApp :: IO ()
startApp = createUserChannel >>= run 8081 . app

app :: EM.EventManager -> Application
app = simpleCors . serve api . server

api :: Proxy API
api = Proxy

server :: EM.EventManager -> Server API
server em = return users :<|> createUsers em :<|> usersChan em

createUsers :: EM.EventManager -> Handler NoContent
createUsers em = do
  result <- liftIO $ EM.eventWrite  "usersChan" (TL.toStrict $ encodeToLazyText users) em  -- have event manager write event to appropriate topic
  case result of
    Right _ -> pure NoContent
    Left e -> throwError $ err500 { errBody = fromStrictBytes . encodeUtf8 . tshow $ e }

usersChan :: EM.EventManager -> Tagged Handler Application
usersChan em = Tagged $ \req resp -> do
    maybeChan <- EM.eventSubscribe "usersChan" em -- subscribe to a topic and get the broadcast channel
    sseChan   <- newChan                          -- create new server sent event channel
    case maybeChan of
      Just (EM.Channel chan) -> do
        _ <- async $ forever $ do                 -- running a process that forever listen to events from broadcast channel
          msg <- atomically $ readTChan chan      -- when an event is captured wrap it in an server sent event type
          writeChan sseChan (toUserSSE msg)       -- and push the event to the server sent channel
        eventSourceAppChan sseChan req resp       -- Maybe this logic should be generalized?
      Nothing   -> do
        resp $ responseLBS status404 [] "Cannot find channel"

  where
      toUserSSE :: Message -> ServerEvent
      toUserSSE msg = ServerEvent
        { eventName = Just $ BB.fromByteString "users"
        , eventId   = Nothing
        , eventData = [BB.fromByteString $ TE.encodeUtf8 msg]
        }

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

createUserChannel :: IO EventManager
createUserChannel = EM.newEventManagerWith ["usersChan"]
