{-# LANGUAGE OverloadedStrings #-}

module IrcFrog.Irc.Network
where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async as Conc

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as Text (encodeUtf8)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM
import qualified Data.Conduit.TMChan as STMC

import qualified Network.IRC.Conduit as IRC
import Conduit as C

import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State

import IrcFrog.Types.Network

temp host port nick = connectNetwork (IrcHostname host) port (IrcUser nick)

connectNetwork :: IrcHostname -> Int -> IrcUser -> IO NetworkEnv
connectNetwork host port nick = do
    sendingQ <- STM.newTBMChanIO 100
    receivingQ <- STM.newTBMChanIO 100
    let env = NetworkEnv {
          _sendingQueue = sendingQ
        , _receivingQueue = receivingQ
        , _hostname = host
        , _port = port
        }

    initialConnState <- STM.newTVarIO Disconnected
    let initialState = NetworkState {
          _nick = nick
        , _connectionState = initialConnState
        }

    -- run the network in a separate thread, and kill it if this thread is killed
    Conc.withAsync (State.runStateT (Reader.runReaderT runNetwork env) initialState) (\a -> Conc.wait a >> return env)
    -- _ <- Conc.concurrently (State.runStateT (Reader.runReaderT runNetwork env) initialState) (logQueue receivingQ)
    -- return env


logQueue :: Show a => STM.TBMChan a -> IO ()
logQueue chan = forever $ do
    msg <- STM.atomically $ STM.readTBMChan chan
    putStrLn $ "Message from server: " ++ show msg


runNetwork :: StatefulNetwork
runNetwork = do
    env <- Reader.ask
    state <- lift State.get
    let IrcHostname hostname = _hostname env
    let port = _port env
    let IrcUser nick = _nick state

    let consumer = C.toConsumer $ C.iterMC (basicHandler state) .| STMC.sinkTBMChan (_receivingQueue env) True
    antiflood <- liftIO $ IRC.floodProtector 1
    let producer = C.toProducer $ STMC.sourceTBMChan (_sendingQueue env) .| antiflood

    let initialise = do
            STM.atomically $ STM.modifyTVar' (_connectionState state) (const Connecting)
            let nickMsg = IRC.Nick (Text.encodeUtf8 nick)
            let userMsg = IRC.rawMessage "USER" [Text.encodeUtf8 nick, "0", "*", Text.encodeUtf8 nick]
            STM.atomically $ STM.writeTBMChan (_sendingQueue env) nickMsg
            STM.atomically $ STM.writeTBMChan (_sendingQueue env) userMsg
    liftIO $ IRC.ircClient port hostname initialise consumer producer

basicHandler :: NetworkState -> (Either ByteString IRC.IrcEvent) -> IO ()
basicHandler state (Right ev) =
    case IRC._source ev of -- (IRC.Numeric n args)) = putStrLn $ "Got numeric arg " ++ show n
      IRC.Server serverName -> handleServerMessage state (IRC._message ev)
      _ -> return ()

basicHandler _ _ = return ()

handleServerMessage :: NetworkState -> IRC.IrcMessage -> IO ()
-- handle welcome message
handleServerMessage state (IRC.Numeric 1 args) = STM.atomically $ STM.modifyTVar' (_connectionState state) (const Connected)
handleServerMessage _ _ = return ()
