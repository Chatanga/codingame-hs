module Codingame.Simulation
    (   Team
    ,   wrapAsProtoTeam
    ,   wrapAsProtoTeamIO
    ,   runSimulation
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad

----------------------------------------------------------------------------------------------------

data Team = Team
    {   team_runStep :: Input -> IO (Team, Output)
    }

wrapAsProtoTeam :: (Init -> a) -> (a -> Input -> (a, Output)) -> (Init -> Team)
wrapAsProtoTeam launch runStep = \init -> Team (runStepWithState (launch init)) where
    runStepWithState state input = do
        let (state', output) = runStep state input
        return (Team (runStepWithState state'), output)

wrapAsProtoTeamIO :: (Init -> IO a) -> (a -> Input -> IO (a, Output)) -> (Init -> Team)
wrapAsProtoTeamIO launch runStep =
    \init -> Team (\input -> launch init >>= (flip runStepWithState input))
    where
    runStepWithState state input = do
        (state', output) <- runStep state input
        return (Team (runStepWithState state'), output)

runSimulation :: [Init -> Team] -> IO ()

runSimulation [protoTeam0, protoTeam1] = do
    protoInit <- runRandomIO $ Init
            -- busterCountPerPlayer
            <$> getRandomR (2, 4)
            -- ghostCount
            <*> getRandomR (10, 20)

    let teams = [protoTeam0 (protoInit 0), protoTeam1 (protoInit 1)]
        entities = []

        loop teams entities = do
            (teams', outputs) <- unzip <$> forM teams (\t -> team_runStep t entities)
            let entities' = entities
            loop teams' entities'

    loop teams entities

runSimulation _ = error "Two teams, no more, no less, are expected."

{-

exeMain1 = runSimulation [golgoth1, golgoth2] where
    golgoth1 = wrapAsProtoTeam createInitialContext (\context -> runTurn context)
    golgoth2 = wrapAsProtoTeamIO (return . createInitialContext) (\context -> return . runTurn context)

-}

----------------------------------------------------------------------------------------------------

type TurnId = Int

data GameReport arena evolution input output = GameReport {
    arena :: arena,
    turns :: [GameTurn evolution input output]
}

type PlayerId = Int

data GameTurn evolution input output = GameTurn {
    contribution :: [(PlayerId, (input, output))],
    evolution :: evolution
}

type Seed = String
type Url = String

data ReportGamePlayer arena evolution input output = ReportGamePlayer {
    report :: GameReport arena evolution input output,
    playerId :: PlayerId
}

createPlayerFromReport :: GameReport arena evolution input output -> PlayerId -> (input -> IO output)
createPlayerFromReport report playerId = const . return . flip evalState 0 $ do
    n <- get
    put (n + 1)
    return $ case lookup playerId (contribution (turns report !! n)) of
        Just (_, o) -> o
        Nothing -> error ("unknown turn " ++ show n)

createPlayerFromAgent :: (String -> IO String) -> (input -> IO output)
createPlayerFromAgent = undefined

data Stage = Situation | Order | Action | Evolution

run :: (GameReport a e i o -> TurnId -> Stage -> IO ())
    -> (Seed -> [(PlayerId, input -> IO output)] -> IO (GameReport arena evolution input output))
    -> (Url -> IO (GameReport arena evolution input output))
    -> Maybe Url
    -> Maybe Seed -- to be verified
    -> [(PlayerId, i -> IO o)] -- to be verified
    -> IO ()
run uiPlayer simulate retrieve urlる seedる players = undefined

-- [GameReport a e i o] -> AnnotatedTimeline
