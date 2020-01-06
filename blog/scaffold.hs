#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.17
  --package directory
  --package time
-}

import System.Directory
import Control.Monad
import Data.Foldable
import Data.Time.Clock
import Data.Time.Format

frontmatter :: String
frontmatter = unlines
  [ "---"
  , "title:"
  , "description:"
  , "author: Riccardo"
  , "cover_image:"
  , "tags:"
  , "  - FunctionalProgramming"
  , "---"
  ]

main :: IO ()
main = do
  today <- formatTime defaultTimeLocale "%F" <$> getCurrentTime
  let fileName = fold ["posts", "/", today, "-"]
  fileExist <- doesFileExist fileName
  when fileExist $ error "file already exists"
  writeFile fileName frontmatter
