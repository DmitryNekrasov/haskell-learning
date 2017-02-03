module Lab02 where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Binary
import Data.Typeable
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

data Team = Team (SendPort ClientApp) deriving (Typeable)
data ClientApp = ClientApp deriving (Typeable)

instance Binary Team where
    put (Team sClientApp) = do
        put (0 :: Word8)
        put sClientApp
    get = do
        t <- get :: Get Word8
        case t of
            0 -> do
                sClientApp <- get
                return (Team sClientApp)

instance Binary ClientApp where
    put ClientApp = putWord8 1
    get = do
        getWord8
        return ClientApp

team :: SendPort Team -> Process ()
team sTeam = do
    (sClientApp, rClientApp) <- newChan
    sendChan sTeam (Team sClientApp)
    liftIO $ putStrLn "Sent a Team!"
    ClientApp <- receiveChan rClientApp
    liftIO $ putStrLn "Got a ClientApp!"
    team sTeam

clientApp :: ReceivePort Team -> Process ()
clientApp rTeam = do
    Team sClientApp <- receiveChan rTeam
    liftIO $ putStrLn "Got a Team!"
    sendChan sClientApp ClientApp
    liftIO $ putStrLn "Sent a ClientApp!"
    clientApp rTeam

ignition :: Process ()
ignition = do
    sTeam <- spawnChannelLocal clientApp
    spawnLocal $ team sTeam
    liftIO $ threadDelay 100000

lab02start = do
    Right transport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node ignition