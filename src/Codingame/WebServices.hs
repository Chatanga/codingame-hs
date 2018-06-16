{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

{-|
Give access to the Codingame web services API from your code. A typical usage is to test your code
against another player into a test session:

@
credentials <- readCredentials \"credentials.json\"
let source = \"main = putStrLn \\\"42\\\"\"
result <- play credentials \"Coders Strike Back\" source [IdeCode, DefaultAi] Nothing
@

-}
module Codingame.WebServices
    ( -- * Complex requests
      submit
    , play
    , submitLatest
    , playLatest
    , readCredentials
      -- * Elementary requests
    , wsLoginSiteV2
    , wsFindGamesPuzzleProgress
    , wsFindPastChallenges
    , wsFindByGameId
    , wsFindInformationByIdAndSaveGame
    , wsGenerateSessionFromPuzzleIdV2
    , wsSubmit
    , wsPlay
      -- * Request data
    , Login (..)
    , User (..)
    , Codingamer (..)
    , GamePuzzleProgress (..)
    , Challenge (..)
    , PublicGameResult (..)
    , GameResult (..)
    , Agent (..)
    , Frame (..)
    , TestSession (..)
    , Submit (..)
    , Play (..)
    , Multi (..)
    , AgentId (..)
    , Credentials (..)
    ) where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Control.Monad.Except
import Data.Aeson -- Need a higher version to generate FromJSON instances.
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import Data.Char
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe
import Data.Ord
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import System.IO
import Text.Printf (printf)

import Codingame.Debug

----------------------------------------------------------------------------------------------------

data ServiceError = ServiceError
    { error_id :: Int
    , error_type :: Maybe String
    , error_message :: String
    } deriving Show

instance FromJSON ServiceError where
    parseJSON (Object v) =
        ServiceError <$>
        (v .: "id") <*>
        (v .:? "type") <*>
        (v .: "message")

----------------------------------------------------------------------------------------------------

data ServiceResult a = ServiceResult
    { result_success :: Maybe a
    , result_error :: Maybe ServiceError
    } deriving Show

instance FromJSON (ServiceResult Int) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult Login) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data Login = Login
    { login_remembered :: Bool
    , login_authenticated :: Bool
    , login_user :: User
    , login_codinGamer :: Codingamer
    , login_languageId :: Int
    , login_userId :: Int
    , login_userEmail :: String
    , login_countryCode :: String
    -- login_newFeatures :: [String]
    -- login_enabledNotifications :: [String]
    -- login_chatToken :: String
    -- login_notificationConfig
    -- login_actionTypes
    } deriving Show

instance FromJSON Login where
    parseJSON (Object v) =
        Login <$>
        (v .: "remembered") <*>
        (v .: "authenticated") <*>
        (v .: "user") <*>
        (v .: "codinGamer") <*>
        (v .: "languageId") <*>
        (v .: "userId") <*>
        (v .: "userEmail") <*>
        (v .: "countryCode")
        -- (v .: "newFeatures") <*>
        -- (v .: "enabledNotifications") <*>
        -- (v .: "chatToken")

data User = User
    { user_id :: Int
    , user_email :: String
    , user_languageId :: Int
    , user_valid :: Bool
    -- user_properties
    } deriving Show

instance FromJSON User where
    parseJSON (Object v) =
        User <$>
        (v .: "id") <*>
        (v .: "email") <*>
        (v .: "languageId") <*>
        (v .: "valid")

data Codingamer = Codingamer
    { codingamer_userId :: Int
    , codingamer_email :: Maybe String
    , codingamer_pseudo :: String
    , codingamer_countryId :: Maybe String
    , codingamer_publicHandle :: Maybe String
    , codingamer_privateHandle :: Maybe String
    , codingamer_registerOrigin :: Maybe String
    , codingamer_enable :: Bool
    , codingamer_userValid :: Bool
    , codingamer_schoolId :: Maybe Int
    , codingamer_rank :: Maybe Int
    , codingamer_avatar :: Int
    , codingamer_cover :: Maybe Int
    , codingamer_creationTime :: Maybe Int
    , codingamer_onlineSince :: Maybe Int
    , codingamer_hiddenFromFriendFinder :: Maybe Bool
    , codingamer_shareAllSolutions :: Maybe Bool
    , codingamer_tagline :: Maybe String
    , codingamer_level :: Maybe Int
    , codingamer_xp :: Maybe Int
    -- codingamer_formValues
    -- codingamer_formCachedValues
    } deriving Show

instance FromJSON Codingamer where
    parseJSON (Object v) =
        Codingamer <$>
        (v .: "userId") <*>
        (v .:? "email") <*>
        (v .: "pseudo") <*>
        (v .:? "countryId") <*>
        (v .:? "publicHandle") <*>
        (v .:? "privateHandle") <*>
        (v .:? "registerLogin") <*>
        (v .: "enable") <*>
        (v .: "userValid") <*>
        (v .:? "schoolId") <*>
        (v .:? "rank") <*>
        (v .: "avatar") <*>
        (v .:? "cover") <*>
        (v .:? "creationTime") <*>
        (v .:? "onlineSince") <*>
        (v .:? "hiddenFromFriendFinder") <*>
        (v .:? "shareAllSolutions") <*>
        (v .:? "tagline") <*>
        (v .:? "level") <*>
        (v .:? "xp")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult [GamePuzzleProgress]) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data GamePuzzleProgress = GamePuzzleProgress
    { progress_id :: Int
    , progress_level :: String
    , progress_rank :: Maybe Int
    -- progress_thumbnailBinaryId :: Maybe Int
    -- progress_previewBinaryId :: Maybe Int
    , progress_title :: String
    -- progress_achievementScore :: Int
    -- progress_validatorScore :: Int
    -- progress_achievementCount :: Int
    -- progress_doneAchievementCount :: Int
    -- progress_totalScore :: Int
    -- progress_unlocked :: Bool
    -- progress_solvedCount :: Int
    -- progress_optimCriteriaId :: Maybe String
    -- progress_creationTime :: Int
    -- progress_topics
    } deriving Show

instance FromJSON GamePuzzleProgress where
    parseJSON (Object v) =
        GamePuzzleProgress <$>
        (v .: "id") <*>
        (v .: "level") <*>
        (v .:? "rank") <*>
        -- (v .:? "thumbnailBinaryId") <*>
        -- (v .:? "previewBinaryId") <*>
        (v .: "title")
        -- (v .: "achievementScore") <*>
        -- (v .: "validatorScore") <*>
        -- (v .: "achievementCount") <*>
        -- (v .: "doneAchievementCount") <*>
        -- (v .: "totalScore") <*>
        -- (v .: "unlocked") <*>
        -- (v .: "solvedCount") <*>
        -- (v .:? "optimCriteriaId") <*>
        -- (v .: "creationTime")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult [Challenge]) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data Challenge = Challenge
    { challenge_title :: String
    , challenge_date :: Int
    , challenge_publicId :: String
    , challenge_cover1Id :: Maybe Int
    , challenge_logoId :: Int
    , challenge_descriptionJson :: Maybe String
    , challenge_endApplicationsDate :: Maybe Int
    , challenge_applicationsClosed :: Maybe Bool
    } deriving Show

instance FromJSON Challenge where
    parseJSON (Object v) =
        Challenge <$>
        (v .: "title") <*>
        (v .: "date") <*>
        (v .: "publicId") <*>
        (v .:? "coverId") <*>
        (v .: "logoId") <*>
        (v .:? "descriptionJson") <*>
        (v .:? "endApplicationsDate") <*>
        (v .:? "applicationsClosed")

instance FromJSON (ServiceResult Challenger) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data Challenger = Challenger
    { challenger_userId :: Int -- Not the same
    , challenger_anonymousIdentifier :: Int
    , challenger_pseudo :: String
    , challenger_online :: Bool
    , challenger_cheater :: Bool
    , challenger_isNewHandleGf :: Bool
    , challenger_progress :: String -- "EQUAL" | ...
    , challenger_testSessionHandle :: String
    , challenger_challengeId :: Int
    , challenger_testsessionId :: Int
    -- codingamer_formValues
    } deriving Show

instance FromJSON Challenger where
    parseJSON (Object v) =
        Challenger <$>
        (v .: "userId") <*>
        (v .: "anonymousIdentifier") <*>
        (v .: "pseudo") <*>
        (v .: "online") <*>
        (v .: "cheater") <*>
        (v .: "isNewHandleGf") <*>
        (v .: "progress") <*>
        (v .: "testSessionHandle") <*>
        (v .: "challengeId") <*>
        (v .: "testsessionId")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult PublicGameResult) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data PublicGameResult = PublicGameResult
    { publicGameResult_viewer :: String
    , publicGameResult_puzzleTitle :: [String]
    , publicGameResult_gameResult :: GameResult
    , publicGameResult_shareable :: Bool
    , publicGameResult_puzzleId :: Int
    } deriving Show

instance FromJSON PublicGameResult where
    parseJSON (Object v) =
        PublicGameResult <$>
        (v .: "viewer") <*>
        (v .: "puzzleTitle") <*>
        (v .: "gameResult") <*>
        (v .: "shareable") <*>
        (v .: "puzzleId")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult GameResult) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data GameResult = GameResult
    { gameResult_gameId :: Int
    , gameResult_refereeInput :: String
    , gameResult_agents :: Maybe [Agent]
    , gameResult_frames :: [Frame]
    , gameResult_ranks :: [Int]
    , gameResult_scores :: [Double]
    } deriving Show

instance FromJSON GameResult where
    parseJSON (Object v) =
        GameResult <$>
        (v .: "gameId") <*>
        (v .: "refereeInput") <*>
        (v .:? "agents") <*>
        (v .: "frames") <*>
        (v .: "ranks") <*>
        (v .: "scores")

data Agent = Agent
    { agent_agentId :: Int
    , agent_index :: Int
    , agent_codingamer :: Codingamer
    , agent_score :: Double
    , agent_valid :: Bool
    } deriving Show

instance FromJSON Agent where
    parseJSON (Object v) =
        Agent <$>
        (v .: "agentId") <*>
        (v .: "index") <*>
        (v .: "codingamer") <*>
        (v .: "score") <*>
        (v .: "valid")

data Frame = Frame
    { frame_agentId :: Int
    , frame_gameInformation :: String
    , frame_view :: String
    , frame_summary :: Maybe String
    , frame_stdout :: Maybe String
    , frame_keyframe :: Maybe Bool
    } deriving Show

instance FromJSON Frame where
    parseJSON (Object v) =
        Frame <$>
        (v .: "agentId") <*>
        (v .: "gameInformation") <*>
        (v .: "view") <*>
        (v .:? "summary") <*>
        (v .:? "stdout") <*>
        (v .:? "keyframe")

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult TestSession) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data TestSession = TestSession
    { testSession_reportReady :: Bool
    , testSession_questionCount :: Int
    , testSession_globalRemainingTime :: Int
    , testSession_handle :: String
    , testSession_direct :: Bool
    } deriving Show

instance FromJSON TestSession where
    parseJSON (Object v) =
        TestSession <$>
        (v .: "reportReady") <*>
        (v .: "questionCount") <*>
        (v .: "globalRemainingTime") <*>
        (v .: "handle") <*>
        (v .: "direct")

----------------------------------------------------------------------------------------------------

data Submit = Submit
    { submit_code :: String
    , submit_programmingLanguageId :: String
    } deriving Show

instance ToJSON Submit where
    toJSON (Submit c pli) = object ["code" .= c, "programmingLanguageId" .= pli]

data Play = Play
    { play_code :: String
    , play_programmingLanguageId :: String
    , play_multi :: Multi
    } deriving Show

instance ToJSON Play where
    toJSON (Play c pli m) = object ["code" .= c, "programmingLanguageId" .= pli, "multi" .= m]

data Multi = Multi
    { multi_agentsIds :: [AgentId]
    , multi_gameOptions :: Maybe String
    -- ^ The game options (the "seed") if any (as provided by gameResult_refereeInput).
    } deriving Show

instance ToJSON Multi where
    toJSON (Multi ai go) = object ["agentsIds" .= ai, "gameOptions" .= go]

data AgentId = IdeCode | DefaultAi | AgentId Int deriving Show

instance ToJSON AgentId where
    toJSON IdeCode = Number (-1)
    toJSON DefaultAi = Number (-2)
    toJSON (AgentId i) = toJSON i

----------------------------------------------------------------------------------------------------

{- | Submit a Haskell source for a given multiplayers challenge. The published source is pushed
into the multiplayers arena and also made available in the IDE (after having refreshed the page
if needed).
-}
submit
    :: Credentials -- ^ A user credentials.
    -> String -- ^ The challenge title.
    -> String -- ^ The source to submit.
    -> IO (Either SessionError Int) -- ^ The error or the submitted agent ID on success.
submit credentials challengeTitle source = runSession $ dumpError $ do
    (userId, gameId) <- connectToMultiGame credentials challengeTitle

    let submitData = Submit source "Haskell"

    testSessionHandle <- testSession_handle <$> wsGenerateSessionFromPuzzleIdV2 (Just userId) gameId
    agentId <- wsSubmit testSessionHandle submitData
    liftIO $ putStrLn $ "Agent ID: " ++ show agentId

    return agentId

{- | Upload a Haskell source into the IDE and play a single game between two agents. Any of these
agent could use the uploaded source, but it is not mandatory. However, even if not used, a source
shall be provided (I don’t know why CG has bound together these two operations).
-}
play
    :: Credentials -- ^ A user credentials.
    -> String -- ^ The challenge title.
    -> String -- ^ The source to submit (even if not used).
    -> [AgentId] -- ^ The agents for the game.
    -> Maybe String -- ^ Optional game options (including the random seed).
    -> IO (Either SessionError GameResult) -- ^ The error or the game result on success.
play credentials challengeTitle source agents gameOptions = runSession $ dumpError $ do
    (userId, gameId) <- connectToMultiGame credentials challengeTitle

    let playData = Play source "Haskell" (Multi agents gameOptions)

    testSessionHandle <- testSession_handle <$> wsGenerateSessionFromPuzzleIdV2 (Just userId) gameId
    gameResult <- wsPlay testSessionHandle playData
    liftIO $ do
        putStrLn $ "Ranks: " ++ show (gameResult_ranks gameResult)
        putStrLn $ "Game options: " ++ show (gameResult_refereeInput gameResult)

    return gameResult

connectToMultiGame :: Credentials -> String -> Session (Int, Int)
connectToMultiGame (Credentials email password) challengeTitle = do
    userId <- login_userId <$> wsLoginSiteV2 email password
    liftIO $ putStrLn $ "User ID: " ++ show userId

    challenges <- filter ((== "multi") . progress_level) <$> wsFindGamesPuzzleProgress (Just userId)

    let challengeTitles = fmap progress_title challenges
        challenge = listToMaybe $ filter ((== challengeTitle) . progress_title) challenges
        error = printf "No challenge named '%s' found in:\n%s" challengeTitle (show challengeTitles)

    gameId <- progress_id <$> asMandatory error challenge

    return (userId, gameId)

{- | Same thing as submit but dedicated to the ongoing challenge (if any).
-}
submitLatest
    :: Credentials -- ^ A user credentials.
    -> String -- ^ The source to submit (even if not used).
    -> IO (Either SessionError Int) -- ^ The error or the submitted agent ID on success.
submitLatest credentials source = runSession $ dumpError $ do
    (userId, testSessionHandle) <- connectToOngoingGame credentials

    let submitData = Submit source "Haskell"

    agentId <- wsSubmit testSessionHandle submitData
    liftIO $ putStrLn $ "Agent ID: " ++ show agentId

    return agentId

{- | Same thing as play but dedicated to the ongoing challenge (if any).
-}
playLatest
    :: Credentials -- ^ A user credentials.
    -> String -- ^ The source to submit (even if not used).
    -> [AgentId] -- ^ The agents for the game.
    -> Maybe String -- ^ Optional game options (including the random seed).
    -> IO (Either SessionError GameResult) -- ^ The error or the game result on success.
playLatest credentials source agents gameOptions = runSession $ dumpError $ do
    (userId, testSessionHandle) <- connectToOngoingGame credentials

    let playData = Play source "Haskell" (Multi agents gameOptions)

    gameResult <- wsPlay testSessionHandle playData
    liftIO $ do
        putStrLn $ "Ranks: " ++ show (gameResult_ranks gameResult)
        putStrLn $ "Game options: " ++ show (gameResult_refereeInput gameResult)

    return gameResult

connectToOngoingGame :: Credentials -> Session (Int, String)
connectToOngoingGame (Credentials email password) = do
    userId <- login_userId <$> wsLoginSiteV2 email password
    liftIO $ putStrLn $ "User ID: " ++ show userId

    challenge <- wsFindXNextVisibleChallenges 1 >>= (asMandatory "No ongoing challenge found" . listToMaybe)
    let (challengeTitle, challengeId) = (challenge_title &&& challenge_publicId) challenge
    liftIO $ putStrLn $ "Challenge title: " ++ show challengeTitle

    testSessionHandle <- challenger_testSessionHandle <$> wsFindChallengerByChallenge challengeId userId
    liftIO $ putStrLn $ "Test session handle: " ++ show testSessionHandle

    return (userId, testSessionHandle)

data Credentials = Credentials
    { credentials_email :: String
    , credentials_password :: String
    } deriving Show

instance FromJSON Credentials where
    parseJSON (Object v) =
        Credentials <$>
        (v .: "email") <*>
        (v .: "password")

{- | Utility function to load credentials from a JSON configuration file.
-}
readCredentials
    :: String -- ^ Configuration file path.
    -> IO Credentials -- ^ The returned credentials.
readCredentials file = do
    fileContent <- BS.readFile file
    let credentials =
            case parseOnly json fileContent of
                Left identificationError -> error identificationError
                Right value -> case fromJSON value of
                    Error conversionError -> error conversionError
                    Success result -> result
    return credentials

asMandatory :: String -> Maybe a -> Session a
asMandatory message Nothing = throwError message
asMandatory _ (Just a) = return a

----------------------------------------------------------------------------------------------------

wsLoginSiteV2
    :: String -- ^ Email.
    -> String -- ^ Password.
    -> Session Login
wsLoginSiteV2 email password = handleResult $ post'
    "https://www.codingame.com/services/CodingamerRemoteService/loginSiteV2"
    [toJSON email, toJSON password, toJSON True]

wsFindGamesPuzzleProgress
    :: Maybe Int -- ^ User ID.
    -> Session [GamePuzzleProgress]
wsFindGamesPuzzleProgress userId = handleResult $ post'
    "https://www.codingame.com/services/PuzzleRemoteService/findGamesPuzzleProgress"
    [toJSON userId]

wsFindPastChallenges
    :: Session [Challenge]
wsFindPastChallenges = handleResult $ post'
    "https://www.codingame.com/services/ChallengeRemoteService/findPastChallenges"
    []

wsFindByGameId
    :: Int -- ^ Replay ID.
    -> Session GameResult
wsFindByGameId replayId = handleResult $ post'
    "https://www.codingame.com/services/gameResultRemoteService/findByGameId"
    [toJSON replayId, Null]

wsFindInformationByIdAndSaveGame
    :: Int -- ^ Replay ID.
    -> Int -- ^ User ID.
    -> Session GameResult
wsFindInformationByIdAndSaveGame replayId userId = handleResult $ post'
    "https://www.codingame.com/services/gameResultRemoteService/findInformationByIdAndSaveGame"
    [toJSON replayId, toJSON userId]

wsGenerateSessionFromPuzzleIdV2
    :: Maybe Int -- ^ User ID.
    -> Int -- ^ Game ID.
    -> Session TestSession
wsGenerateSessionFromPuzzleIdV2 userId gameId = handleResult $ post'
    "https://www.codingame.com/services/PuzzleRemoteService/generateSessionFromPuzzleIdV2"
    [toJSON userId, toJSON gameId]

wsFindXNextVisibleChallenges
    :: Int -- An index I guess (starting at 1)
    -> Session [Challenge]
wsFindXNextVisibleChallenges index = handleResult $ post'
    "https://www.codingame.com/services/ChallengeRemoteService/findXNextVisibleChallenges"
    [toJSON index]

wsFindChallengerByChallenge
    :: String -- ^ Game name.
    -> Int -- ^ User ID.
    -> Session Challenger
wsFindChallengerByChallenge challengeTitle userId = handleResult $ post'
    "https://www.codingame.com/services/ChallengeCandidateRemoteService/findChallengerByChallenge"
    [toJSON challengeTitle, toJSON userId]

wsSubmit
    :: String -- ^ Test session handle.
    -> Submit
    -> Session Int
wsSubmit testSessionHandle submit = handleResult $ post'
    "https://www.codingame.com/services/TestSessionRemoteService/submit"
    [toJSON testSessionHandle, toJSON submit, Null] -- ^ Clarify the 3rd argument meaning.

wsPlay
    :: String -- ^ Test session handle.
    -> Play
    -> Session GameResult
wsPlay testSessionHandle play = handleResult $ post'
    "https://www.codingame.com/services/TestSessionRemoteService/play"
    [toJSON testSessionHandle, toJSON play]

{-
wsGetUserArenaDivisionRoomRankingByTestSessionHandle testSessionHandle = handleResult $ post'
    "https://www.codingame.com/services/LeaderboardsRemoteService/getUserArenaDivisionRoomRankingByTestSessionHandle"
    [toJSON testSessionHandle, "global"]

wsGetArenaDivisionRoomLeaderboard testSessionHandle = handleResult $ post'
    "https://www.codingame.com/services/LeaderboardsRemoteService/getArenaDivisionRoomLeaderboard"
    ["an object", "some id", Null]

wsFindLastBattlesAndProgressByTestSessionHandle testSessionHandle = handleResult $ post'
    "https://www.codingame.com/services/gamesPlayersRankingRemoteService/findLastBattlesAndProgressByTestSessionHandle"
    [toJSON testSessionHandle, Null]
-}

post' :: (Show a, FromJSON a) => String -> [Value] -> Session a
post' url parameters = post url (encode parameters)

----------------------------------------------------------------------------------------------------

type Cookies = [(LBS.ByteString, LBS.ByteString)]

data SessionState = SessionState
    { sessionState_cookies :: Cookies
    }

type SessionError = String

type Session = StateT SessionState (ExceptT SessionError IO)

runSession :: Session a -> IO (Either SessionError a)
runSession session = runExceptT (evalStateT session (SessionState []))

dumpError :: Session a -> Session a
dumpError session = catchError session $ \e -> do
    liftIO $ hPutStrLn stderr e
    throwError e

----------------------------------------------------------------------------------------------------

-- | Every response carries a success or a (logical) error. This function extract the first or
-- transform the latter into an error.
handleResult :: Show a => Session (ServiceResult a) -> Session a
handleResult session = do
    result <- session
    case result of
        (ServiceResult Nothing (Just error)) -> throwError (error_message error)
        (ServiceResult (Just success) Nothing) -> return success
        (ServiceResult Nothing Nothing) -> throwError "Nothing returned"

-- | Do a POST request, updating the session cookies along the way.
post
    :: (FromJSON a, Show a)
    => String -- ^ The web service URL.
    -> LBS.ByteString -- ^ The request body as a JSON content.
    -> Session a -- ^ The returned session.
post url body = do
    cookies <- sessionState_cookies <$> get
    manager <- liftIO $ newManager tlsManagerSettings
    initialRequest <- liftIO $ parseUrlThrow url
    let request = initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders =
                [ ("Content-Type", "application/json; charset=utf-8")
                , ("Cookie", encodeCookies cookies)
                ]
            }
    let thirtySeconds = 30000000
    response <- liftIO $ httpLbs request{responseTimeout = responseTimeoutMicro thirtySeconds} manager
    let cookies' = removeOlders (decodeSetCookies response ++ cookies)
        removeOlders = List.nubBy (\x y -> EQ == comparing fst x y)
    put (SessionState cookies')
    case parseResponse response of
        Left e -> throwError e
        Right a -> return a

encodeCookies :: Cookies -> BS.ByteString
encodeCookies = LBS.toStrict . LBS.intercalate ";" . fmap showCookie where
    showCookie (name, value) = name `LBS.append` "=" `LBS.append` value

decodeSetCookies :: Response LBS.ByteString -> Cookies
decodeSetCookies response = response
    & responseHeaders
    & filter ((== "Set-Cookie") . fst)
    & fmap (head . LBS.split ';' . LBS.fromStrict . snd)
    & fmap (LBS.split '=')
    & fmap (\(k:v:_) -> (k, v))
    & filter (flip elem ["JSESSIONID", "AWSELB"] . fst)

parseResponse :: FromJSON a => Response LBS.ByteString -> Either String a
parseResponse response =
    if statusCode (responseStatus response) == 200
        then case parseOnly json (LBS.toStrict $ responseBody (_trace "response" response)) of
            Left identificationError -> Left identificationError
            Right value -> case fromJSON (_trace "value" value) of
                Error conversionError -> Left conversionError
                Success result -> Right result
        else Left (LBS.unpack $ responseBody response)
