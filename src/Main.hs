{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Network.IRC.Client as IRC
import qualified Network.IRC.Client.Types as IRC
import Network.IRC.Conduit.Internal.Messages (IrcMessage)

import System.Environment (getArgs)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM

import IrcFrog.TestView
import IrcFrog.View
import qualified IrcFrog.State as State

import qualified IrcFrog.Types as T

main :: IO ()
main = do
    [nick] <- getArgs
    let (freenodeHost, freenodePort) = ("chat.freenode.net", 7000)
    freenodeConn <- IRC.connectWithTLS' IRC.stdoutLogger "chat.freenode.net" 7000 1
    freenodeInChan <- STM.atomically $ STM.newTBMChan 30 :: IO (STM.TBMChan IRC.UnicodeEvent)
    let network = T.Network
            { T.host = freenodeHost
            , T.port = freenodePort
            , T.nick = Text.pack nick
            , T.outboundChan = IRC._sendqueue freenodeConn
            , T.inboundChan = freenodeInChan
            , T.channels = []
            }
    _ <- Async.async $ runIrcConnection freenodeConn (Text.pack nick) freenodeInChan
    Conc.threadDelay 2000000 *> joinChan "#gougoutest" (IRC._sendqueue freenodeConn)
    testView
    -- run "chat.freenode.net" 7000 (T.pack nick)

runIrcConnection
    :: IRC.ConnectionConfig ()
    -> Text
    -> STM.TBMChan IRC.UnicodeEvent
    -> IO ()
runIrcConnection connection nick inChan =
    let
        cfg = IRC.defaultIRCConf nick
        cfg' = cfg {
                IRC._eventHandlers = forwardEvent : IRC._eventHandlers cfg
            }
        forwardEvent = IRC.EventHandler
            { IRC._description = "Forward received message to an STM channel"
            , IRC._matchType = IRC.EEverything
            , IRC._eventFunc = liftIO . STM.atomically . STM.writeTBMChan inChan
            }
    in
        IRC.start connection cfg'

run :: B.ByteString -> Int -> Text -> IO ()
run host port nick = do
    conn <- IRC.connectWithTLS' IRC.stdoutLogger host port 1
    let cfg = IRC.defaultIRCConf nick
    let sendQueue = IRC._sendqueue conn
    -- Async.async $ pollConnState (IRC._connState conn)
    _ <- Async.async $ sendStuff 2000000 sendQueue
    IRC.start conn cfg

logger :: IRC.Origin -> B.ByteString -> IO ()
logger origin msg = putStrLn $ "From " <> show origin <> " got message: " <> B.toString msg

sendStuff :: Int -> STM.TBMChan IrcMessage -> IO ()
sendStuff delay queue = do
    Conc.threadDelay delay
    let msg = IRC.Join "#gougoutest"
    STM.atomically $ STM.writeTBMChan queue msg
    STM.atomically $ STM.writeTBMChan queue (IRC.Privmsg "#gougoutest" (Right "hello"))

joinChan chanName queue =
    STM.atomically $ STM.writeTBMChan queue (IRC.Join chanName)
