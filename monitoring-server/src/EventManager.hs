module EventManager where 

import RIO 
import RIO.HashMap as HM 

type Topic = Text 
type Message = Text 

newtype Channel = Channel (TChan Message)

newtype EventManager = EventManager (HM.HashMap Topic Channel)

data EventManagerError = TopicNotFound 

getChannel :: Topic -> EventManager -> Maybe Channel
getChannel topic (EventManager em) = HM.lookup topic em

pushEvent :: Topic -> Message -> EventManager -> IO (Either EventManagerError ())
pushEvent topic message mgr
    | Just (Channel chan) <- getChannel topic mgr = atomically $ writeTChan chan message $> pure ()
    | otherwise                                   = pure $ Left TopicNotFound

