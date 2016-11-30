{-# LANGUAGE OverloadedStrings #-}

module IrcFrog.Irc.Network
where

import Control.Monad (forever)
import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Conc

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Text (Text)

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TBMChan as STM
import qualified Data.Conduit.TMChan as STMC

import qualified Network.IRC.Conduit as IRC
import Conduit


-- given a hostname, port and nick, will attempt to connect to the irc network
-- and return a pair of channel, one for inbound irc events, and another one to send messages.
manageNetwork :: ByteString -> Int -> Text -> IO () -- (STM.TBMChan IRC.UnicodeEvent, STM.TBMChan IRC.UnicodeMessage)
manageNetwork hostname port nick = do
    rcvQueue <- STM.atomically $ STM.newTBMChan 30

    let consumer = toConsumer $ STMC.sinkTBMChan rcvQueue True
    let producer = do
            lift $ print "producing ?"
            yield (IRC.Nick $ Text.encodeUtf8 nick)

    sendQueue <- STM.atomically $ STM.newTBMChan 30
    antiflood <- liftIO $ IRC.floodProtector 1
    let producer = toProducer $ STMC.sourceTBMChan sendQueue .| antiflood

    let initialise = do
            let nickMsg = IRC.rawMessage "NICK" [Text.encodeUtf8 nick]
            let userMsg = IRC.rawMessage "USER" [Text.encodeUtf8 nick, "0", "*", Text.encodeUtf8 nick]
            let joinMsg = IRC.Join "#gougoutest"
            putStrLn "writing message: "
            print nickMsg
            putStrLn ""
            print userMsg
            STM.atomically $ STM.writeTBMChan sendQueue nickMsg
            STM.atomically $ STM.writeTBMChan sendQueue userMsg
            STM.atomically $ STM.writeTBMChan sendQueue joinMsg

    _ <- Conc.forkIO $ forever $ do
        msg <- STM.atomically $ STM.readTBMChan rcvQueue
        putStrLn $ "got message from server: " ++ show msg

    _ <- Conc.async $ do
        Conc.threadDelay 500000
        print "sending message"
        let msg = IRC.Privmsg "#gougoutest" (Right "yo !")
        STM.atomically $ STM.writeTBMChan sendQueue msg

    putStrLn "starting"
    IRC.ircClient port hostname initialise consumer producer
