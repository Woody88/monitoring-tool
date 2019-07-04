module Config where

import           Control.Concurrent                   (ThreadId)
import           Control.Monad.Metrics                (Metrics)
import qualified Database.MySQL.Base                  as MySQL
import qualified Katip
import           Network.Wai                          as Wai
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger as RequestLogger
import           RIO

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config =
  Config
    { configBinLogConn :: !MySQL.MySQLConn
    , configEnv        :: !Environment
    , configMetrics    :: !Metrics
    , configEkgServer  :: !ThreadId
    , configLogEnv     :: !Katip.LogEnv
    , configPort       :: !Warp.Port
    }

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Wai.Middleware
setLogger Test        = id
setLogger Development = RequestLogger.logStdoutDev
setLogger Production  = RequestLogger.logStdout
