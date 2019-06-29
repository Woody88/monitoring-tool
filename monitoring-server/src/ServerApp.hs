module ServerApp where

import           Config
import           Control.Monad.Except  (ExceptT, MonadError)
import           Control.Monad.Logger
import           Control.Monad.Metrics (MonadMetrics, getMetrics)
import           Katip
import           Logger                (liftKatip)
import           RIO
import           Servant               (ServantErr)

-- ServerApp contains the monad that all Servant handlers will be running in.
newtype ServerAppT m a =
    ServerAppT
    { runServerApp :: ReaderT Config (ExceptT ServantErr m) a
    } deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

type ServerApp = ServerAppT IO

instance Monad m => MonadMetrics (ServerAppT m) where
    getMetrics = asks Config.configMetrics

instance MonadIO m => Katip (ServerAppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv f = local (\s -> s { configLogEnv = f (configLogEnv s)})

instance MonadIO m => MonadLogger (ServerAppT m) where
    monadLoggerLog = liftKatip logMsg

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = liftKatip logMsg
