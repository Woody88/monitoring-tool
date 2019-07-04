module EventManager
    ( EventManager(..)
    , Channel(..)
    , Message
    , newEventManager
    , newEventManagerWith
    , newTopic
    , eventWrite
    , eventSubscribe
    )
    where

import           RIO
import           RIO.HashMap as HM

type Topic = Text
type Message = Text

newtype Channel = Channel (TChan Message)

newtype EventManager = EventManager (HM.HashMap Topic Channel)

data EventManagerError = TopicNotFound deriving Show

newEventManager :: EventManager
newEventManager = EventManager mempty

newEventManagerWith :: [Topic] -> IO EventManager
newEventManagerWith = foldM (\em topic -> newChannel >>= (\chan -> pure $ addChannel topic chan em)) newEventManager

newChannel :: IO Channel
newChannel =  Channel <$> newTChanIO

newTopic :: Topic -> EventManager -> IO EventManager
newTopic topic em = (\chan -> addChannel topic chan em) <$> newChannel

getChannel :: Topic -> EventManager -> Maybe Channel
getChannel topic (EventManager em) = HM.lookup topic em

addChannel :: Topic -> Channel -> EventManager -> EventManager
addChannel topic channel (EventManager em) = EventManager $ HM.insert topic channel em

eventWrite :: Topic -> Message -> EventManager -> IO (Either EventManagerError ())
eventWrite topic message em
    | Just (Channel chan) <- getChannel topic em = atomically $ writeTChan chan message $> pure ()
    | otherwise                                   = pure $ Left TopicNotFound

eventSubscribe :: Topic -> EventManager -> IO (Maybe Channel)
eventSubscribe topic em
    | Just (Channel chan) <- getChannel topic em = Just . Channel <$> atomically (cloneTChan chan)
    | otherwise                                  = pure Nothing


