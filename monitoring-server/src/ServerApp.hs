module ServerApp where 

import Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Metrics                (MonadMetrics, getMetrics)
import RIO
import Servant (ServantErr)
import Config

-- ServerApp contains the monad that all Servant handlers will be running in. 
newtype ServerAppT m a = 
    ServerAppT 
    { runServerApp :: ReaderT Config (ExceptT ServantErr m) a 
    } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type ServerApp = ServerAppT IO

instance Monad m => MonadMetrics (ServerAppT m) where 
    getMetrics = asks Config.configMetrics