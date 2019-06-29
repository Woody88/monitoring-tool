module Logger where

import qualified Control.Monad.Logger  as Logger
import           Katip                 (ColorStrategy (..), LogEnv, LogStr,
                                        Namespace (..), Severity (..),
                                        Verbosity (..), defaultScribeSettings,
                                        initLogEnv, logStr, mkHandleScribe,
                                        registerScribe)
import           RIO
import qualified System.IO             as IO
import           System.Log.FastLogger as FastLogger

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- Katip.mkHandleScribe ColorIfTerminal IO.stdout DebugS V2
    env <- Katip.initLogEnv "servant-persistent" "production"
    Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

fromLevel :: Logger.LogLevel -> Severity
fromLevel Logger.LevelDebug     = DebugS
fromLevel Logger.LevelInfo      = InfoS
fromLevel Logger.LevelWarn      = WarningS
fromLevel Logger.LevelError     = ErrorS
fromLevel (Logger.LevelOther _) = NoticeS

-- | Transforms Katip logMsg into monadLoggerLog to be used inside
-- MonadLogger monad
liftKatip :: (ToLogStr msg)  =>
         (Katip.Namespace -> Severity -> Katip.LogStr -> m ()) ->
          Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> msg -> m ()
liftKatip f _ src lvl msg = f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
