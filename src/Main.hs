{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Monoid ((<>))
import Control.Monad (forever, forM_)

-- import Control.Exception (bracket)
-- import Control.Monad.IO.Class (liftIO)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.UTF8 as B
--
-- import Data.Text (Text)
-- import qualified Data.Text as Text
--
-- import qualified Network.IRC.Client as IRC
-- import qualified Network.IRC.Client.Types as IRC
-- import Network.IRC.Conduit.Internal.Messages (IrcMessage)
--
-- import System.Environment (getArgs)
-- import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Conc
import qualified Control.Concurrent.Chan as Chan

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM

--
-- import qualified Brick.Main as BM
-- import Brick.AttrMap (attrMap)
-- import qualified Graphics.Vty as V
-- import qualified Data.Default as Default
--
-- import IrcFrog.TestView
-- import qualified IrcFrog.View as View
-- import qualified IrcFrog.State as State
--
-- import qualified IrcFrog.Types as T
import IrcFrog.Types.Connection
import IrcFrog.Types.Message

import qualified IrcFrog.Connection as Connection
import IrcFrog.Types.Network

import qualified IrcFrog.Types.Client as ClientTypes
import qualified IrcFrog.Client as Client

main :: IO ()
main = do
    testEnv <-
        Connection.makeConnectionEnv
            (NetworkHostname "irc.freenode.net")
            6667
            (IrcUser "testingstuff")
    appChan <- Chan.newChan
    Conc.withAsync
        (Conc.concurrently (Connection.connectNetwork testEnv) (forwardMessages testEnv appChan)) $
        \stuff -> do
            _ <- Client.run appChan
            Conc.cancel stuff
            putStrLn "Tearing down everything"

-- Tag every message coming from
forwardMessages :: ConnectionEnv -> Chan.Chan ClientTypes.AppEvent -> IO ()
forwardMessages env chan =
    let inChan = ceReceivingQueue env
        nid = ceNetworkId env
    in forever $
       do msg <- STM.atomically $ STM.readTBMChan inChan
          case msg of
              Just m -> Chan.writeChan chan $ ClientTypes.ConnectionEvent nid m
              Nothing -> return ()
-- main = do
--     t1 <- Conc.async $ do
--         putStrLn "In child thread, spawning a subtask"
--         bracket (Conc.async noisy) (\a -> putStrLn "Cancelling noisy task" >> Conc.cancel a) (\a -> Conc.wait a >> putStrLn "child done")
--         Conc.threadDelay 10000000
--         putStrLn "child thread done"
--     Conc.threadDelay 1500000
--     putStrLn "Killing child thread"
--     Conc.cancel t1
--     putStrLn "Child Killed, now going to sleep for a while"
--     Conc.threadDelay 2000000
--     putStrLn "done"
-- noisy = forever $ (putStrLn "yo!") >> Conc.threadDelay 1000000
-- main :: IO ()
-- main = do
--     [nick] <- getArgs
--     let (freenodeHost, freenodePort) = ("chat.freenode.net", 7000)
--     -- freenodeConn <- IRC.connectWithTLS "chat.freenode.net" 7000 1
--     freenodeInChan <- STM.atomically $ STM.newTBMChan 30 :: IO (STM.TBMChan IRC.UnicodeEvent)
--
--     tmpChan <- STM.atomically $ STM.newTBMChan 10
--
--     let network = T.Network
--             { T.host = freenodeHost
--             , T.port = freenodePort
--             , T.nick = Text.pack nick
--             -- , T.outboundChan = IRC._sendqueue freenodeConn
--             , T.outboundChan = tmpChan
--             , T.inboundChan = freenodeInChan
--             , T.channels = []
--             }
--     -- _ <- Async.async $ runIrcConnection freenodeConn (Text.pack nick) freenodeInChan
--     -- Conc.threadDelay 2000000 *> joinChan "#gougoutest" (IRC._sendqueue freenodeConn)
--     startApp network
--     -- testView
--     -- run "chat.freenode.net" 7000 (T.pack nick)
--
--
-- startApp :: T.Network -> IO ()
-- startApp initialNetwork = do
--     let app = BM.App { BM.appDraw = View.render
--                      , BM.appStartEvent = return
--                      , BM.appHandleEvent = State.handleEvent
--                      , BM.appAttrMap = const (attrMap V.defAttr [])
--                      , BM.appLiftVtyEvent = id
--                      , BM.appChooseCursor = State.appChooseCursor
--                      }
--     chan <- Conc.newChan
--     let initialState = State.initialState { T.networks = [initialNetwork] }
--     _ <- BM.customMain (V.mkVty Default.def) chan app initialState
--     putStrLn "yoo"
--
--
-- runIrcConnection
--     :: IRC.ConnectionConfig ()
--     -> Text
--     -> STM.TBMChan IRC.UnicodeEvent
--     -> STM.TBMChan IRC.UnicodeEvent
--     -> IO ()
-- runIrcConnection connection nick receivingChan sendingChan = do
--     let cfg = IRC.defaultIRCConf nick
--     let forwardEvent = IRC.EventHandler {
--           IRC._description = "Forward received message to an STM channel"
--         , IRC._matchType = IRC.EEverything
--         , IRC._eventFunc = liftIO . STM.atomically . STM.writeTBMChan receivingChan
--         }
--     let cfg' = cfg {
--         IRC._eventHandlers = forwardEvent : IRC._eventHandlers cfg
--     }
--     Async.withAsync (IRC.start connection cfg') $ \_ -> forever $ do
--         line <- getLine
--         putStrLn $ "got line: " ++ line
--
--
-- run :: B.ByteString -> Int -> Text -> IO ()
-- run host port nick = do
--     conn <- IRC.connectWithTLS' IRC.stdoutLogger host port 1
--     let cfg = IRC.defaultIRCConf nick
--     let sendQueue = IRC._sendqueue conn
--     -- Async.async $ pollConnState (IRC._connState conn)
--     _ <- Async.async $ sendStuff 2000000 sendQueue
--     IRC.start conn cfg
--
-- logger :: IRC.Origin -> B.ByteString -> IO ()
-- logger origin msg = putStrLn $ "From " <> show origin <> " got message: " <> B.toString msg
--
-- sendStuff :: Int -> STM.TBMChan IrcMessage -> IO ()
-- sendStuff delay queue = do
--     Conc.threadDelay delay
--     let msg = IRC.Join "#gougoutest"
--     STM.atomically $ STM.writeTBMChan queue msg
--     STM.atomically $ STM.writeTBMChan queue (IRC.Privmsg "#gougoutest" (Right "hello"))
--
-- joinChan chanName queue =
--     STM.atomically $ STM.writeTBMChan queue (IRC.Join chanName)
