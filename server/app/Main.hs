{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson (eitherDecode', encode)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as U4
import qualified Logic
import qualified Network.WebSockets as WS
import WSMessage (WSMessage (..))
import qualified WSResponse

type GameID = UUID.UUID

type ClientID = UUID.UUID

data Clients = Clients
  { cids :: Set.Set ClientID, -- creo que es superfluo
    usernames :: Map.Map ClientID Text,
    connections :: Map.Map ClientID WS.Connection
  }

data Games = Games
  { gids :: Set.Set GameID, -- esto también
    names :: Map.Map GameID Text,
    hosts :: Map.Map GameID ClientID,
    opponents :: Map.Map GameID ClientID,
    spectators :: Map.Map GameID [ClientID],
    state :: Map.Map GameID Logic.GameState
  }

data ServerState = ServerState
  { games :: Games,
    clients :: Clients
  }


nextGameID :: IO GameID
nextGameID = U4.nextRandom

nextClientId :: IO ClientID
nextClientId = U4.nextRandom

newServerState :: ServerState
newServerState = ServerState newGamesState newClientsState

newGamesState :: Games
newGamesState =
  Games
    { gids = Set.empty,
      names = Map.empty,
      hosts = Map.empty,
      opponents = Map.empty,
      spectators = Map.empty,
      state = Map.empty
    }

newClientsState :: Clients
newClientsState =
  Clients
    { cids = Set.empty,
      usernames = Map.empty,
      connections = Map.empty
    }


gameExists :: GameID -> ServerState -> Bool
gameExists gid = Set.member gid . gids . games

canJoinGame :: GameID -> ServerState -> Bool
canJoinGame gid ss@ServerState {games} =
  not $ Map.member gid $ opponents games

addGame :: ClientID -> Text -> ServerState -> IO ServerState
addGame host gname ss@ServerState {games} =
  let updater g@Games {gids, hosts, names} gid =
        g
          { gids = Set.insert gid gids,
            names = Map.insert gid gname names,
            hosts = Map.insert gid host hosts
          }
   in nextGameID
        >>= ( \gid ->
                pure $
                  ss {games = updater games gid}
            )

addClient :: ClientID -> WS.Connection -> Text -> ServerState -> ServerState
addClient cid cxn name ss@ServerState {clients} =
  let updater c@Clients {cids, usernames, connections} =
        c
          { cids = Set.insert cid cids,
            usernames = Map.insert cid name usernames,
            connections = Map.insert cid cxn connections
          }
   in ss {clients = updater clients}

-- | Must also remove any games they are involved in
removeClient :: ClientID -> ServerState -> ServerState
removeClient cid ss@ServerState {clients} =
  let updater c@Clients {cids, usernames, connections} =
        c
          { cids = Set.delete cid cids,
            usernames = Map.delete cid usernames,
            connections = Map.delete cid connections
          }
   in ss {clients = updater clients}


joinClientToGame :: ClientID -> GameID -> ServerState -> Either Text ServerState
joinClientToGame cid gid ss@ServerState {clients, games} =
  if canJoinGame gid ss
    then
      Right $
        ss
          { games =
              games
                { opponents = Map.insert gid cid $ opponents games,
                  state = Map.insert gid (Logic.PlacingArmy Logic.newPlayer Logic.newPlayer) $ state games
                }
          }
    else Left $ "Cannot join game" <> UUID.toText gid

broadcast :: WSResponse.WSResponse -> ServerState -> IO ()
broadcast message serverState = do
  print message
  forM_ (Map.elems $ connections $ clients serverState) $ flip WS.sendTextData (encode message)

main :: IO ()
main = do
  state <- newMVar newServerState
  T.putStrLn "Accepting requests!"
  WS.runServer "127.0.0.1" 9160 $ server state

-- |
-- - server checks and routes incoming connection requests
server :: MVar ServerState -> WS.ServerApp
server state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg <- eitherDecode' <$> WS.receiveData conn :: IO (Either String WSMessage)
    ServerState {games, clients} <- readMVar state
    case msg of
      Right (SetName name) -> do
        cid <- nextClientId
        flip finally (disconnect cid) $ do
          modifyMVar_ state $ \s -> do
            let s' = addClient cid conn name s
            broadcast (WSResponse.UserJoined name) s
            broadcast (WSResponse.UserList $ userList s') s'
            WS.sendTextData conn ("Welcome to the lobby!" :: Text)
            WS.sendTextData conn (encode $ WSResponse.NameSet name)
            pure s'
          lobby cid state
        where
          disconnect cid = do
            s <- modifyMVar state $ \s -> do
              let s' = removeClient cid s
               in return (s', s')
            broadcast (WSResponse.Failed $ name <> " disconnected") s
      _ -> WS.sendTextData conn (pack $ show msg)

lobby :: ClientID -> MVar ServerState -> IO ()
lobby cid state = forever $ do
  ss <- readMVar state
  let conn = Map.lookup cid $ connections $ clients ss
  let name = Map.lookup cid $ usernames $ clients ss
  maybe
    (pure ())
    ( \(conn, name) -> do
        msg <- eitherDecode' <$> WS.receiveData conn :: IO (Either String WSMessage)
        T.putStrLn ("Received lobby message from " <> name <> ": " <> pack (show msg))
        case msg of
          Right ListGames ->
            readMVar state >>= WS.sendTextData conn . encode . WSResponse.GameList . gameList
          Right ListUsers ->
            readMVar state >>= WS.sendTextData conn . encode . WSResponse.UserList . userList
          Right (HostGame gameName) -> do
            modifyMVar_ state $ \s -> do
              let s' = addGame cid gameName s
              s' >>= broadcast (WSResponse.GameAdded gameName) -- ("New game '" <> gameName <> "' added! " <> gameList s') s'
              s'
            lobby cid state
          Right (JoinGame gid) -> do
            modifyMVar_ state $ \s -> do
              pure $ fromRight s $ joinClientToGame cid gid s
            playing cid gid state
          Right (SetName newName) -> do
            modifyMVar_ state $ \s@ServerState {clients} -> do
              pure $
                s
                  { clients =
                      clients
                        { usernames = Map.insert cid newName $ usernames clients
                        }
                  }
          _ -> do WS.sendTextData conn (pack $ show msg) :: IO ()
    )
    ((,) <$> conn <*> name)

playing :: ClientID -> GameID -> MVar ServerState -> IO ()
playing cid gid state = forever $ do
  ss <- readMVar state
  let conn = Map.lookup cid $ connections $ clients ss
  let name = Map.lookup cid $ usernames $ clients ss
  maybe
    (pure ())
    ( \(conn, name) -> do
        msg <- eitherDecode' <$> WS.receiveData conn :: IO (Either String WSMessage)
        T.putStrLn ("Received game room message from " <> name <> ": " <> pack (show msg))
        case msg of
          Right (SetPieces pieces) -> do
            modifyMVar_ state $ \s@ServerState {games} -> do
              pure s
          Right (Play _) -> do
            modifyMVar_ state $ \s -> do
              pure s
          Right Forfeit -> do
            modifyMVar_ state $ \s -> do
              pure s
          _ -> do WS.sendTextData conn (pack $ show msg) :: IO ()
    )
    ((,) <$> conn <*> name)

userList :: ServerState -> [Text]
userList state = do
  Map.elems (usernames $ clients state)

-- | Only returns open games atm
gameList :: ServerState -> [WSResponse.Match]
gameList ServerState {games, clients} =
  Map.foldrWithKey
    (\k n l -> WSResponse.Match n (UUID.toText k) : l)
    []
    $ Map.intersection
      (names games)
      (unopposedHosts games)

unopposedHosts :: Games -> Map.Map GameID ClientID
unopposedHosts Games {hosts, opponents} =
  Map.difference hosts opponents
