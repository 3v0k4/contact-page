{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))
import System.Directory

data Opts =
  Opts
    { creds :: String
    , post :: String
    }

main :: IO ()
main = do
  cs <- fmap (<> "/.cred.toml") getHomeDirectory
  execParser (opts cs) >>= (\Opts{..} -> tweet creds post)
  where
    opts cs = info (parser cs <**> helper)
      (  fullDesc
      <> progDesc "Shares on Twitter a new blog POST using CREDS for authentication"
      )

parser :: FilePath -> Parser Opts
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
