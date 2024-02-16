module Main where

import Control.Monad (when)
import Data.Functor
import Data.List (permutations)
import Data.Text (Text)
import Data.Text qualified as T
import OVD.Types
import System.Environment
import Telegram.Bot.API.MakingRequests (defaultTelegramClientEnv)
import Telegram.Bot.Simple.BotApp
import TgBot qualified

permutFullNames :: [PerPerson Text] -> [PerPerson [Text]]
permutFullNames perPerson =
  concat $
    perPerson
      <&> \(PerPerson (Person fullName i) uids city loc) ->
        permutations (T.words fullName)
          <&> \fullNameWords -> PerPerson (Person fullNameWords i) uids city loc

-- -- | Returns (names not found in Ruz, names with info from Ruz)
-- withRuz :: MonadIO io => [PerPerson Text] -> io ([Text], [PerPerson Text])
-- withRuz perPerson = if null perPerson then pure ([], []) else liftIO do
--     pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
--                              , pgTotal = toInteger (length perPerson)
--                              }
--     fmap concat . partitionEithers @Text @[PerPerson Text]
--         <$> withTaskGroup 4 \g -> flip (mapConcurrently g) (permutFullNames perPerson)
--             \(PerPerson (Person fullNameWords _) uids city loc) -> do
--                 ruzResults <- query Ruz fullNameWords <* tick pb
--                 pure $ if null ruzResults then
--                             Left (T.unwords fullNameWords)
--                         else Right $ ruzResults
--                                         <&> \(QueryResult lbl descr) ->
--                                               PerPerson (Person lbl descr) uids city loc

-- withVK :: Text -> [PerPerson Text] -> IO [PerPerson Text]
-- withVK accessToken people = if null people then pure [] else do
--     pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
--                              , pgTotal = toInteger (length people)
--                              }
--     startEvalMemoT $
--         for people \(PerPerson (Person fullName' info) _ city loc) ->
--             -- Most VK users have only surname and name
--             let fullName = dropPatronymic fullName'
--              in (memo (findVKAccount accessToken) fullName <* liftIO (tick pb)) <&>
--                     (\uids -> PerPerson (Person fullName' info) uids city loc)

-- fromOVD :: String -> IO ()
-- fromOVD ovdUrl = do
--     dataDir <- lookupEnv "DATA_DIR" <&> maybe Prelude.id (<>)
--     !accessTokens <- getAccessTokens
--     let allCsv = dataDir "all.csv"
--     let freshCsv = dataDir "fresh.csv"

--     (oldNames :: [Text]) <- T.readFile allCsv <&> T.lines -- <&> Set.fromList
--     let (oldNamesWords :: [Set Text]) = oldNames <&> T.words <&> Set.fromList
--     parsed <- fromMaybe (trace "ERROR PARSING OVD" []) <$> scrapeURL ovdUrl byCityInBody
--     let flattened = parsed
--                   & flattenByCity
--                   & trimFullNames
--                   & filter \perPerson ->
--                             let fullNameWords = perPerson & perPersonFullName & T.words & Set.fromList
--                              in not (any (Set.isSubsetOf fullNameWords) oldNamesWords)
--     (noRuz, fromRuz) <- displayConsoleRegions $ withRuz flattened
--     final <- displayConsoleRegions $ withVK accessTokens fromRuz
--     let freshContent = T.Lazy.unlines $ T.Lazy.fromStrict . perPersonToCsv <$> final
--     let allContent = T.Lazy.unlines $ T.Lazy.fromStrict <$>
--             Set.toList (Set.fromList (fmap (dropPatronymic . perPersonFullName) final ++ noRuz))
--     T.Lazy.appendFile allCsv allContent
--     T.Lazy.writeFile freshCsv freshContent

noTokenInstruction :: String
noTokenInstruction =
  "Нет токена ВК. Перейдите по ссылке "
    ++ "https://oauth.vk.com/authorize?client_id=8094092&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=327682&response_type=token&v=5.131 "
    ++ "Из адресной строки скопируйте access_token, затем export VK_TOKEN=$access_token"

getAccessTokens :: IO [Text]
getAccessTokens = do
  ats <- lookupEnv "VK_TOKEN" <&> maybe (Prelude.error noTokenInstruction) (T.split (',' ==) . T.pack)
  when (null ats) do
    fail "VK_TOKEN is empty"
  pure ats

usage :: String
usage =
  unlines
    [ "Usage: ruz-finder COMMAND"
    , "COMMAND = ovd URL | find_in_vk FULL_NAME | find_in_vk - OUT_FILE | bot"
    ]

main :: IO ()
main =
  getArgs
    >>=
    {-displayConsoleRegions .-} \case
      [] -> Prelude.error usage
      -- ["ovd"] -> Prelude.error "missing URL"
      -- ["ovd", ovdUrl] -> fromOVD ovdUrl
      -- ["find_in_vk"] -> Prelude.error "missing FULL_NAME"
      -- ["find_in_vk", "-"] -> Prelude.error "missing OUT_FILE"
      -- ["find_in_vk", inFile, outFile] -> do
      --     !accessTokens <- getAccessTokens
      --     lns <- T.lines <$> case inFile of
      --         "-" -> T.getContents
      --         inFile' -> T.readFile inFile'
      --     matchesCsv <- findInVKMany accessTokens lns <&> fmap personMatchToCsv
      --     let outContent = T.unlines matchesCsv
      --     case outFile of
      --         "-" -> T.putStrLn outContent
      --         outFile' -> T.writeFile outFile' outContent
      -- ["find_in_vk", fullName] -> do
      --     !accessTokens <- getAccessTokens
      --     findInVKMany accessTokens [T.pack fullName] >>= traverse_ (T.putStrLn . personMatchToCsv)
      ["bot"] -> do
        !accessTokens <- getAccessTokens
        token <- getEnvToken "TG_TOKEN"
        defaultTelegramClientEnv token >>= startBot_ (TgBot.bot accessTokens)
      command : opts -> Prelude.error $ "command " ++ command ++ " unknown (options: " ++ unwords opts ++ ")"
