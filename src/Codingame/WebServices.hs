{-# LANGUAGE OverloadedStrings, FlexibleInstances, RecordWildCards #-}

{-|
Give access to the Codingame web services API from your code. A typical usage is to test your code
against another player into a test session:

@
credentials <- readCredentials \"credentials.json\"
let source = \"main = putStrLn \\\"42\\\"\"
playInIDE credentials OngoingChallenge source [IdeCode, DefaultAi] Nothing
@

-}

module Codingame.WebServices
    ( submitToArena
    , playInIDE
    , getLastBattles
    , getGameResult
    , readCredentials
    , Codingamer (..)
    , GameResult (..)
    , Agent (..)
    , Frame (..)
    , AgentId (..)
    , Player (..)
    , Battle (..)
    , ChallengeTitle (..)
    , Credentials (..)
    , SessionError
    ) where

import Control.Monad.State
    ( gets, evalStateT, MonadIO(liftIO), MonadState(put), StateT )
import Control.Monad.Except
    ( runExceptT,
      MonadError(throwError, catchError),
      ExceptT )
import Data.Aeson
    ( encode,
      json,
      (.:),
      (.:?),
      fromJSON,
      object,
      FromJSON(parseJSON),
      Result(Success, Error),
      Value(Number, Object, Null),
      KeyValue((.=)),
      ToJSON(toJSON) )
import Data.Attoparsec.ByteString ( parseOnly )
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString as BS
import Data.Function ( (&) )
import qualified Data.List as List
import Data.Maybe ( listToMaybe )
import Data.Ord ( comparing )
import Network.HTTP.Client
    ( responseTimeoutMicro,
      httpLbs,
      newManager,
      parseUrlThrow,
      Request(method, requestBody, requestHeaders, responseTimeout),
      RequestBody(RequestBodyLBS),
      Response(responseHeaders, responseStatus, responseBody) )
import Network.HTTP.Client.TLS ( tlsManagerSettings )
import Network.HTTP.Types.Status (statusCode)
import System.IO ( stderr, hPutStrLn )
import Text.Printf (printf)

import qualified Debug.Trace as Trace

trace :: Show a => String -> a -> a
trace message x = Trace.trace (message ++ " = " ++ show x) x

{-
Implementation note: instances of the ToJSON and FromJSON classes could be automatically generated
provided our JSON data would derive the Generic class. However, we don’t this for 2 reasons:
-   The DeriveGeneric extension induces a notable compilation cost.
-   Property names should be the same as whose used in JSON, forcing us to add the
    DuplicateRecordFields extension. Unfortunately, the implementation of this extension is not
    perfect at this time and it causes a lot of ambiguous name resolutions.
-}

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
    { login_user :: User
    , login_codinGamer :: Codingamer
    , login_languageId :: Int
    , login_userId :: Int
    , login_userEmail :: String
    , login_countryCode :: String
    } deriving Show

instance FromJSON Login where
    parseJSON (Object v) =
        Login <$>
        (v .: "user") <*>
        (v .: "codinGamer") <*>
        (v .: "languageId") <*>
        (v .: "userId") <*>
        (v .: "userEmail") <*>
        (v .: "countryCode")

data User = User
    { user_id :: Int
    , user_email :: String
    , user_languageId :: Int
    } deriving Show

instance FromJSON User where
    parseJSON (Object v) =
        User <$>
        (v .: "id") <*>
        (v .: "email") <*>
        (v .: "languageId")
        -- (v .: "valid")

data Codingamer = Codingamer
    { codingamer_userId :: Int
    , codingamer_email :: Maybe String
    , codingamer_pseudo :: String
    , codingamer_countryId :: Maybe String
    , codingamer_publicHandle :: Maybe String
    , codingamer_privateHandle :: Maybe String
    , codingamer_registerOrigin :: Maybe String
    , codingamer_enable :: Bool
    , codingamer_schoolId :: Maybe Int
    , codingamer_rank :: Maybe Int
    , codingamer_avatar :: Maybe Int
    , codingamer_cover :: Maybe Int
    , codingamer_creationTime :: Maybe Int
    , codingamer_onlineSince :: Maybe Int
    , codingamer_hiddenFromFriendFinder :: Maybe Bool
    , codingamer_shareAllSolutions :: Maybe Bool
    , codingamer_tagline :: Maybe String
    , codingamer_level :: Maybe Int
    , codingamer_xp :: Maybe Int
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
        (v .:? "schoolId") <*>
        (v .:? "rank") <*>
        (v .:? "avatar") <*>
        (v .:? "cover") <*>
        (v .:? "creationTime") <*>
        (v .:? "onlineSince") <*>
        (v .:? "hiddenFromFriendFinder") <*>
        (v .:? "shareAllSolutions") <*>
        (v .:? "tagline") <*>
        (v .:? "level") <*>
        (v .:? "xp")

instance ToJSON Codingamer where
    toJSON Codingamer{..} =
        object
            [ "userId" .= codingamer_userId
            , "email" .= codingamer_email
            , "pseudo" .= codingamer_pseudo
            , "countryId" .= codingamer_countryId
            , "publicHandle" .= codingamer_publicHandle
            , "privateHandle" .= codingamer_privateHandle
            , "registerOrigin" .= codingamer_registerOrigin
            , "enable" .= codingamer_enable
            , "schoolId" .= codingamer_schoolId
            , "rank" .= codingamer_rank
            , "avatar" .= codingamer_avatar
            , "cover" .= codingamer_cover
            , "creationTime" .= codingamer_creationTime
            , "onlineSince" .= codingamer_onlineSince
            , "hiddenFromFriendFinder" .= codingamer_hiddenFromFriendFinder
            , "shareAllSolutions" .= codingamer_shareAllSolutions
            , "tagline" .= codingamer_tagline
            , "level" .= codingamer_level
            , "xp" .= codingamer_xp
            ]

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
    , progress_title :: String
    } deriving Show

instance FromJSON GamePuzzleProgress where
    parseJSON (Object v) =
        GamePuzzleProgress <$>
        (v .: "id") <*>
        (v .: "level") <*>
        (v .:? "rank") <*>
        (v .: "title")

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

instance ToJSON GameResult where
    toJSON GameResult{..} =
        object
            [ "gameId" .= gameResult_gameId
            , "refereeInput" .= gameResult_refereeInput
            , "agents" .= gameResult_agents
            , "frames" .= gameResult_frames
            , "ranks" .= gameResult_ranks
            , "scores" .= gameResult_scores
            ]

data Agent = Agent
    { agent_agentId :: Int
    , agent_index :: Int
    , agent_codingamer :: Maybe Codingamer
    , agent_score :: Maybe Double
    , agent_valid :: Maybe Bool
    } deriving Show

instance FromJSON Agent where
    parseJSON (Object v) =
        Agent <$>
        (v .: "agentId") <*>
        (v .: "index") <*>
        (v .:? "codingamer") <*>
        (v .:? "score") <*>
        (v .:? "valid")

instance ToJSON Agent where
    toJSON Agent{..} =
        object
            [ "agentId" .= agent_agentId
            , "index" .= agent_index
            , "codingamer" .= agent_codingamer
            , "score" .= agent_score
            , "valid" .= agent_valid
            ]

data Frame = Frame
    { frame_agentId :: Int
    , frame_gameInformation :: String
    , frame_view :: String
    , frame_summary :: Maybe String
    , frame_stdout :: Maybe String
    , frame_stderr :: Maybe String
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
        (v .:? "stderr") <*>
        (v .:? "keyframe")

instance ToJSON Frame where
    toJSON Frame{..} =
        object
            [ "agentId" .= frame_agentId
            , "gameInformation" .= frame_gameInformation
            , "view" .= frame_view
            , "summary" .= frame_summary
            , "stdout" .= frame_stdout
            , "stderr" .= frame_stderr
            , "keyframe" .= frame_keyframe
            ]

----------------------------------------------------------------------------------------------------

instance FromJSON (ServiceResult [Battle]) where
    parseJSON (Object v) =
        ServiceResult <$>
        (v .:? "success") <*>
        (v .:? "error")

data Battle = Battle
    { battle_players :: [Player]
    , battle_gameId :: Int
    , battle_done :: Bool
    } deriving Show

instance FromJSON Battle where
    parseJSON (Object v) =
        Battle <$>
        (v .: "players") <*>
        (v .: "gameId") <*>
        (v .: "done")

instance ToJSON Battle where
    toJSON Battle{..} =
        object
            [ "players" .= battle_players
            , "gameId" .= battle_gameId
            , "done" .= battle_done
            ]

data Player = Player
    { player_playerAgentId :: Int
    , player_position :: Int
    , player_userId :: Int
    , player_nickname :: String
    , player_publicHandle :: String
    , player_avatar :: Maybe Int
    } deriving Show

instance FromJSON Player where
    parseJSON (Object v) =
        Player <$>
        (v .: "playerAgentId") <*>
        (v .: "position") <*>
        (v .: "userId") <*>
        (v .: "nickname") <*>
        (v .: "publicHandle") <*>
        (v .:? "avatar")

instance ToJSON Player where
    toJSON Player{..} =
        object
            [ "playerAgentId" .= player_playerAgentId
            , "position" .= player_position
            , "userId" .= player_userId
            , "nickname" .= player_nickname
            , "publicHandle" .= player_publicHandle
            , "avatar" .= player_avatar
            ]

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

data ChallengeTitle = OngoingChallenge | ChallengeTitle String deriving Show

{- | Submit a Haskell source for a given multiplayers challenge. The published source is pushed
into the multiplayers arena and also made available in the IDE (after having refreshed the page
if needed).
-}
submitToArena
    :: Credentials -- ^ A user credentials.
    -> ChallengeTitle -- ^ The challenge title.
    -> String -- ^ The source to submit.
    -> IO (Either SessionError Int) -- ^ The error or the submitted agent ID on success.
submitToArena credentials challenge source = runSession $ dumpError $ do
    (userId, testSessionHandle) <- connectToChallenge credentials challenge
    wsSubmit testSessionHandle (Submit source "Haskell")

{- | Upload a Haskell source into the IDE and play a single game between multiple agents. Any of these
agent could use the uploaded source, but it is not mandatory. However, even if not used, a source
shall be provided (I don’t know why CG has bound together these two operations).
-}
playInIDE
    :: Credentials -- ^ A user credentials.
    -> ChallengeTitle -- ^ The challenge title.
    -> String -- ^ The source to submit (even if not used).
    -> [AgentId] -- ^ The agents for the game.
    -> Maybe String -- ^ Optional game options (including the random seed).
    -> IO (Either SessionError GameResult) -- ^ The error or the game result on success.
playInIDE credentials challenge source agents gameOptions = runSession $ dumpError $ do
    (userId, testSessionHandle) <- connectToChallenge credentials challenge
    wsPlay testSessionHandle (Play source "Haskell" (Multi agents gameOptions))

{- | Retrieve the last battles (how many by the way?) waged by the player's bot in the arena.
Note that some of these battles could be still in progress.
-}
getLastBattles
    :: Credentials -- ^ A user credentials.
    -> ChallengeTitle -- ^ The challenge title.
    -> IO (Either SessionError [Battle]) -- ^ The error or the list of battles on success.
getLastBattles credentials challenge = runSession $ dumpError $ do
    (userId, testSessionHandle) <- connectToChallenge credentials challenge
    wsFindLastBattlesByTestSessionHandle testSessionHandle

{- | Download a specific game result.
-}
getGameResult
    :: Credentials -- ^ A user credentials.
    -> ChallengeTitle -- ^ The challenge title.
    -> Int -- ^ The game ID.
    -> IO (Either SessionError GameResult) -- ^ The error or the game result on success.
getGameResult credentials challenge gameId = runSession $ dumpError $ do
    (userId, _) <- connectToChallenge credentials challenge
    {- The testSessionHandle is not needed, which is understandable since game result are
    persisted and no bound to a given test session. The user ID is mandatory in order to
    get the corresponding stderr data, but why? The server still requires us to be connected
    and check that the game result has been created by the provided user ID. Can multiple
    user ID be associated to the same account? If it was an agent ID, I would understood...
    Maybe it is just a way to ask for the stderr of all my agents, whereas leaving it at
    null means it’s accessed as a public result without the need of being authenticated?
    -}
    wsFindByGameId gameId (Just userId)

connectToChallenge
    :: Credentials -- ^ A user credentials.
    -> ChallengeTitle -- ^ The challenge title.
    -> Session (Int, String) -- ^ The error or the (user ID, test session handle) on success.

connectToChallenge (Credentials email password) OngoingChallenge = do
    userId <- login_userId <$> wsLoginSiteV2 email password

    challenge <- wsFindXNextVisibleChallenges 1 >>= (asMandatory "No ongoing challenge found" . listToMaybe)
    challenger <- wsFindChallengerByChallenge (challenge_publicId challenge) userId

    return (userId, challenger_testSessionHandle challenger)

connectToChallenge (Credentials email password) (ChallengeTitle challengeTitle) = do
    userId <- login_userId <$> wsLoginSiteV2 email password

    ongoingChallenge <- wsFindXNextVisibleChallenges 1 >>= (asMandatory "No ongoing challenge found" . listToMaybe)
    pastChallenges <- wsFindPastChallenges (Just userId)

    let challengePublicIds = map challenge_publicId (ongoingChallenge : pastChallenges)
        error = printf "No challenge '%s' found in:\n%s" challengeTitle (show challengePublicIds)

    challengeId <- asMandatory error (List.find (== challengeTitle) challengePublicIds)

    challenger <- wsFindChallengerByChallenge challengeId userId

    return (userId, challenger_testSessionHandle challenger)

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

wsFindPastChallenges
    :: Maybe Int -- ^ User ID.
    -> Session [Challenge]
wsFindPastChallenges userId = handleResult $ post'
    "https://www.codingame.com/services/ChallengeRemoteService/findPastChallenges"
    [toJSON userId]

wsFindByGameId
    :: Int -- ^ Replay ID.
    -> Maybe Int -- ^ User ID.
    -> Session GameResult
wsFindByGameId replayId userId = handleResult $ post'
    "https://www.codingame.com/services/gameResultRemoteService/findByGameId"
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
    [toJSON testSessionHandle, toJSON submit, Null] -- Clarify the 3rd argument meaning.

wsPlay
    :: String -- ^ Test session handle.
    -> Play
    -> Session GameResult
wsPlay testSessionHandle play = handleResult $ post'
    "https://www.codingame.com/services/TestSessionRemoteService/play"
    [toJSON testSessionHandle, toJSON play]

wsFindLastBattlesByTestSessionHandle
    :: String -- ^ Test session handle.
    -> Session [Battle]
wsFindLastBattlesByTestSessionHandle testSessionHandle = handleResult $ post'
    "https://www.codingame.com/services/gamesPlayersRankingRemoteService/findLastBattlesByTestSessionHandle"
    [toJSON testSessionHandle, Null] -- Clarify the 3rd argument meaning.

post' :: (Show a, FromJSON a) => String -> [Value] -> Session a
post' url parameters = post url (encode parameters)

----------------------------------------------------------------------------------------------------

type Cookies = [(LBS.ByteString, LBS.ByteString)]

newtype SessionState = SessionState
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

-- | Do a POST request, updating the session cookies along the way.
post
    :: (FromJSON a, Show a)
    => String -- ^ The web service URL.
    -> LBS.ByteString -- ^ The request body as a JSON content.
    -> Session a -- ^ The returned session.
post url body = do
    cookies <- gets sessionState_cookies
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
    response <- liftIO $ httpLbs request{ responseTimeout = responseTimeoutMicro thirtySeconds } manager
    let cookies' = removeOlders (decodeSetCookies response ++ cookies)
        removeOlders = List.nubBy (\x y -> EQ == comparing fst x y)
    put (SessionState cookies')
    case parseResponse response of
        Left e -> throwError e
        Right a -> return a

encodeCookies :: Cookies -> BS.ByteString
encodeCookies = LBS.toStrict . LBS.intercalate ";" . map showCookie where
    showCookie (name, value) = name `LBS.append` "=" `LBS.append` value

decodeSetCookies :: Response LBS.ByteString -> Cookies
decodeSetCookies response = response
    & responseHeaders
    & filter ((== "Set-Cookie") . fst)
    & map (head . LBS.split ';' . LBS.fromStrict . snd)
    & map (LBS.split '=')
    & map (\(k:v:_) -> (k, v))
    & filter (flip elem ["AWSALBCORS", "AWSALB", "cgSession", "rememberMe"] . fst)

parseResponse :: FromJSON a => Response LBS.ByteString -> Either String a
parseResponse response =
    if statusCode (responseStatus response) == 200
        then case parseOnly json (LBS.toStrict $ responseBody response) of
            Left identificationError -> Left (trace "identificationError" identificationError)
            Right value -> case fromJSON value of
                Error conversionError -> Left (trace "conversionError" conversionError)
                Success result -> Right result
        else Left (LBS.unpack $ responseBody response)
