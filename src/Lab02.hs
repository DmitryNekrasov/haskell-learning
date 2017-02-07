module Lab02 where

import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan

team chanIn chanClientApp = do
    putStrLn "Team submit Problem"
    writeChan chanClientApp "team"

    niceTry <- readChan chanIn
    putStrLn "Team get monitor"

clientApp chanIn chanMonitor chanTeam chanController = do
    niceTry <- readChan chanIn
    putStrLn "Client App send problem"
    writeChan chanController "client_app"
    putStrLn "Client App requested monitor"
    writeChan chanMonitor "client_app"

    niceTry <- readChan chanIn
    putStrLn "Client App get monitor"
    putStrLn "Client App send monitor to team"
    writeChan chanTeam "client_app"

monitor chanIn chanClientApp = do
    niceTry <- readChan chanIn
    case niceTry of
        "client_app" -> do
            putStrLn "Monitor returned to client app"
            writeChan chanClientApp "monitor"
        "controller" -> do
            putStrLn "Monitor updated"

controller chanIn chanMonitor chanTestingSystem = do
    niceTry <- readChan chanIn
    putStrLn "Controller get Problem"
    putStrLn "Controller send Problem to Testing System"
    writeChan chanTestingSystem "controller"

    niceTry <- readChan chanIn
    putStrLn "Controller get verdict"
    putStrLn "Controller update monitor"
    writeChan chanMonitor "controller"

testingSystem chanIn chanController = do
    niceTry <- readChan chanIn
    putStrLn "Testing System get Problem"
    putStrLn "Testing System send verdict to controller"
    writeChan chanController "testing_system"

forkCreator action = forkIO $ forever action

lab02start :: IO ()
lab02start = do
    teamChan <- newChan
    clientAppChan <- newChan
    monitorChan <- newChan
    controllerChan <- newChan
    testingSystemChan <- newChan

    forkCreator $ team teamChan clientAppChan
    forkCreator $ clientApp clientAppChan monitorChan teamChan controllerChan
    forkCreator $ monitor monitorChan clientAppChan
    forkCreator $ controller controllerChan monitorChan testingSystemChan
    forkCreator $ testingSystem testingSystemChan controllerChan

    getLine
    return ()