module Database
    ( MySQL.MySQLConn
    , MySQL.close
    , getMySQLConn
    , subscribeBinlogEvent
    , startBinlogListener
    )
    where

import           Config
import qualified Database.MySQL.Base   as MySQL
import qualified Database.MySQL.BinLog as MySQL
import           EventManager          as EM
import           RIO
import           System.Environment    (lookupEnv)
import qualified System.IO.Streams     as Streams
import          Control.Monad.Loops  (whileJust_)

data DatabaseError
    = DBConfigNotFound
    | DBOtherError SomeException
    deriving Show

instance Exception DatabaseError

startBinlogListener :: MySQL.MySQLConn -> EventManager -> IO ()
startBinlogListener dbConn em = do
    em'           <- newTopic "database" em
    void $ async $ subscribeBinlogEvent dbConn em'


subscribeBinlogEvent :: MySQL.MySQLConn -> EventManager -> IO ()
subscribeBinlogEvent conn em = 
    MySQL.getLastBinLogTracker conn >>= \case
        Just tracker -> do
            es <- MySQL.decodeRowBinLogEvent =<< MySQL.dumpBinLog conn 1024 tracker False
            whileJust_ (Streams.read es) $ \v -> do 
                    sayShow v
        Nothing -> error "can't get latest binlog position"


getMySQLConn :: Config.Environment -> IO (MySQL.Greeting, MySQL.MySQLConn)
getMySQLConn = MySQL.connectDetail <=< getMysqlConfig

getMysqlConfig :: Config.Environment -> IO MySQL.ConnectInfo
getMysqlConfig = \case
    Test        ->
        return $ MySQL.defaultConnectInfo
            { MySQL.ciUser = "mtooltest"
            , MySQL.ciPassword = "mtooltest"
            , MySQL.ciDatabase = "mtooltest"
            }

    Production  -> getProdCfg >>= maybe (throwIO DBConfigNotFound) return

    Development ->
        return $ MySQL.defaultConnectInfo
            { MySQL.ciUser = "mtooldev"
            , MySQL.ciPassword = "mtooldev"
            , MySQL.ciDatabase = "mtooldev"
            }

getProdCfg :: IO (Maybe MySQL.ConnectInfo)
getProdCfg = do
    mdbUser     <- lookupEnv "DBUSER"
    mdbPassword <- lookupEnv "DBPASSWORD"
    mdbName     <- lookupEnv "DBNAME"
    pure $ do
        dbUser     <- mdbUser
        dbPassword <- mdbPassword
        dbName     <- mdbName
        pure $ MySQL.defaultConnectInfo
            { MySQL.ciUser     = fromString dbUser
            , MySQL.ciPassword = fromString dbPassword
            , MySQL.ciDatabase = fromString dbName
            }
