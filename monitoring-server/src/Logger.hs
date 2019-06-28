module Logger where

import           Katip     (ColorStrategy (..), LogEnv, Severity (..),
                            Verbosity (..), defaultScribeSettings, initLogEnv,
                            mkHandleScribe, registerScribe)
import           RIO
import qualified System.IO as IO

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal IO.stdout DebugS V2
    env <- initLogEnv "servant-persistent" "production"
    registerScribe "stdout" handleScribe defaultScribeSettings env
