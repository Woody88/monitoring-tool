module Application where

import           API                      (app, createUserChannel)
import           Config                   (Config (..), Environment (..),
                                           setLogger)
import           Control.Concurrent       (killThread)
import           Control.Exception        as Exception
import qualified Control.Monad.Metrics    as Metrics
import           Data.Text.IO             as TIO
import           Database
import           Katip
import           Lens.Micro               ((^.))
import           Logger
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Metrics      as WaiMetrics
import           RIO
import           System.Remote.Monitoring as SRM
import           Util

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = Exception.bracket getAppSettings cleanAppResources runApp'
  where
    runApp' config = Warp.run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
    em <- createUserChannel
    _ <- Database.startBinlogListener (configBinLogConn cfg) em   -- get binlog thread
    waiMetrics <- registerWaiMetrics (configMetrics cfg ^. Metrics.metricsStore)
    let logger = Config.setLogger (configEnv cfg)
    TIO.putStrLn $ "Running on port: " <> (tshow . configPort $ cfg)
    return . logger . metrics waiMetrics $ app em

-- | Allocates resources for 'Config'
getAppSettings :: IO Config
getAppSettings = do
    port                <- Util.lookupSetting "PORT" 8081
    env                 <- Util.lookupSetting "ENV" Development
    logEnv              <- Logger.defaultLogEnv
    ekgServer           <- SRM.forkServer "localhost" 8000
    (_, binlogConn)     <- Database.getMySQLConn env
    let store = SRM.serverMetricStore ekgServer
    _ <- WaiMetrics.registerWaiMetrics store
    metr <- Metrics.initializeWith store
    pure Config
        { configBinLogConn = binlogConn
        , configEnv = env
        , configMetrics = metr
        , configLogEnv = logEnv
        , configPort = port
        , configEkgServer = SRM.serverThreadId ekgServer
        }

-- | Takes care of cleaning up 'Config' resources
cleanAppResources :: Config -> IO ()
cleanAppResources cfg = do
    putStrLn "Cleanning ressources..."
    _ <- Katip.closeScribes (configLogEnv cfg)
    --_ <- Database.close (configBinLogConn cfg)  -- close database binlog connection, it hangs the thread when I run it
    -- Monad.Metrics does not provide a function to destroy metrics store
    -- so, it'll hopefully get torn down when async exception gets thrown
    -- at metrics server process
    killThread (configEkgServer cfg)
    putStrLn "Cleanning done..."
    pure ()

