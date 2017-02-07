module Lab02 where

import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan

team chanIn chanClientApp = do
    putStrLn "----- Team check monitor"
    writeChan chanClientApp "team"

    niceTry <- readChan chanIn

    putStrLn "Team get monitor"

clientApp chanIn chanMonitor chanTeam = do
    niceTry <- readChan chanIn
    putStrLn "Client App requested monitor"
    writeChan chanMonitor "client_app"

    niceTry <- readChan chanIn
    putStrLn "Client App get monitor"
    putStrLn "Client App send monitor to team"
    writeChan chanTeam "client_app"

monitor chanIn chanClientApp = do
    niceTry <- readChan chanIn
    putStrLn "Monitor returned to client app"
    writeChan chanClientApp "monitor"

forkCreator action = forkIO $ forever action

lab02start :: IO ()
lab02start = do

    teamChan <- newChan
    clientAppChan <- newChan
    monitorChan <- newChan

    forkCreator $ team teamChan clientAppChan
    forkCreator $ clientApp clientAppChan monitorChan teamChan
    forkCreator $ monitor monitorChan clientAppChan

    getLine
    return ()