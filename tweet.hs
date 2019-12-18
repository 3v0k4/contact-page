{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory
import Data.Frontmatter
import Data.Yaml (Value)
import Data.ByteString
import Data.ByteString.Char8
import Data.Aeson
import GHC.Generics
import System.FilePath.Posix
import Web.Tweet
import Data.Foldable
import Data.List

data Opts =
  Opts
    { creds :: String
    , post :: String
    }

data Front =
  Front
    { title :: String
    , description :: String
    , tags :: [String]
    } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
  cs <- fmap (<> "/.cred.toml") getHomeDirectory
  execParser (opts cs) >>= (\Opts{..} -> tweet creds post)
  where
    opts cs = info (parser cs <**> helper)
      (  fullDesc
      <> progDesc "Shares on Twitter a new blog POST using CREDS for authentication"
      )

parser :: FilePath -> Options.Applicative.Parser Opts
parser creds = Opts
      <$> strOption
         (  long "creds"
         <> metavar "CREDS"
         <> help "Path to creds .toml file"
         <> value creds
         <> showDefault
         )
      <*> argument str
         (  metavar "POST"
         <> help "Path to blog post file"
         )

mkTweet :: FilePath -> Front -> String
mkTweet path Front{..} = fold [title, " 📒 ", description, "\n\n", htags, "\n\n", url]
  where
    base = "https://odone.io/posts"
    name = System.FilePath.Posix.takeBaseName path
    url = fold [base, "/", name, ".html"]
    htags = Data.List.intercalate " " $ fmap ('#':) tags

tweet :: String -> FilePath -> IO ()
tweet creds path =
  parseYamlFrontmatter <$> Data.ByteString.readFile path
    >>= \case
      Done _post frontmatter ->
        basicTweet (mkTweet path frontmatter) creds >> pure ()
      e ->
        error $ show e
