#!/usr/bin/env stack
{- stack
  script
  --resolver lts-14.17
  --package directory
  --package time
-}

import Control.Monad
import Data.Foldable
import Data.Time.Clock
import Data.Time.Format
import System.Directory

frontmatter :: String
frontmatter =
  unlines
    [ "---",
      "title: TODO",
      "description: TODO",
      "published: false",
      "author: Riccardo",
      "cover_image: https://odone.io/images/TODO.png",
      "tags:",
      "  - Functional Programming",
      "---"
    ]

main :: IO ()
main = do
  today <- formatTime defaultTimeLocale "%F" <$> getCurrentTime
  let fileName = fold ["posts", "/", today, "-todo.md"]
  fileExist <- doesFileExist fileName
  when fileExist $ error "file already exists"
  writeFile fileName frontmatter
