module Application where

import           API                      (app, createUserChannel)
import           Config                   (Config (..), Environment (..),
                                           setLogger)
import           Control.Concurrent       (killThread)
import           Control.Exception        as Exception
import qualified Control.Monad.Metrics    as Metrics
import           Data.Text.IO             as TIO
import           Katip
import           Lens.Micro               ((^.))
import           Logger
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Metrics      as WaiMetrics
import           RIO
import           Safe                     (readMay)
import           System.Environment       (lookupEnv)
import           System.Remote.Monitoring as SRM

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
    TIO.putStrLn $ "Running on port: " <> (tshow . configPort $ cfg)
    waiMetrics <- registerWaiMetrics (configMetrics cfg ^. Metrics.metricsStore)
    let logger = Config.setLogger (configEnv cfg)
    logger . metrics waiMetrics . app <$> createUserChannel

-- | Allocates resources for 'Config'
getAppSettings :: IO Config
getAppSettings = do
    port <- lookupSetting "PORT" 8081
    env  <- lookupSetting "ENV" Development
    logEnv <- Logger.defaultLogEnv
    ekgServer <- SRM.forkServer "localhost" 8000
    let store = SRM.serverMetricStore ekgServer
    _ <- WaiMetrics.registerWaiMetrics store
    metr <- Metrics.initializeWith store
    pure Config
        { configEnv = env
        , configMetrics = metr
        , configLogEnv = logEnv
        , configPort = port
        , configEkgServer = SRM.serverThreadId ekgServer
        }

-- | Takes care of cleaning up 'Config' resources
cleanAppResources :: Config -> IO ()
cleanAppResources cfg = do
    _ <- Katip.closeScribes (configLogEnv cfg)
    -- Monad.Metrics does not provide a function to destroy metrics store
    -- so, it'll hopefully get torn down when async exception gets thrown
    -- at metrics server process
    killThread (configEkgServer cfg)
    pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing  -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]
