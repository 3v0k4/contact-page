{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Data.Binary (Binary)
import Data.Foldable (fold, traverse_)
import Data.Monoid (mappend)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Hakyll
import System.Environment (getEnvironment)
import System.FilePath (replaceExtension)

--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "odone.io",
      feedDescription = "Rambling on software as a learning tool",
      feedAuthorName = "Riccardo Odone",
      feedAuthorEmail = "",
      feedRoot = "https://odone.io"
    }

previewHost' :: String
previewHost' = previewHost defaultConfiguration

previewPort' :: Int
previewPort' = previewPort defaultConfiguration

previewUrl :: String
previewUrl = fold ["http://", previewHost', ":", show previewPort', "/"]

main :: IO ()
main = do
  env <- getEnvironment
  let configuration = defaultConfiguration {previewHost = previewHost', previewPort = previewPort'}
  hakyllWith configuration $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "robots.txt" $ do
      route idRoute
      compile copyFileCompiler
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    matchMetadata "posts/*" isPublished $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
    matchMetadata "posts/*" (not . isPublished) $ do
      let draftPath = ("drafts/" <>) . (`replaceExtension` "html") . toFilePath
      route . customRoute $ draftPath
      let putDraftUrl path =
            traverse_
              (unsafeCompiler . putStrLn)
              [ "----DRAFT----",
                (previewUrl <>) . draftPath . itemIdentifier $ path,
                "-------------"
              ]
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
          >>= (\x -> putDraftUrl x >> pure x)
    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllPublished env "posts/*"
        let archiveCtx =
              listField "posts" postCtx (return posts)
                `mappend` constField "title" "Archives"
                `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx =
              constField "title" "Home"
                `mappend` defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
    match "templates/*" $ compile templateBodyCompiler
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = mconcat [bodyField "description", defaultContext]
        posts <-
          fmap (take 10) . recentFirst
            =<< loadAllSnapshotsPublished "posts/*" "content"
        renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

loadAllPublished :: (Binary a, Typeable a) => [(String, String)] -> Pattern -> Compiler [Item a]
loadAllPublished env pattern_ = if isDevelopmentEnv env then all else published
  where
    all = loadAll pattern_
    published = publishedIds pattern_ >>= traverse load
    isDevelopmentEnv env = lookup "HAKYLL_ENV" env == Just "development"

loadAllSnapshotsPublished :: (Binary a, Typeable a) => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshotsPublished pattern_ snapshot = publishedIds pattern_ >>= traverse (`loadSnapshot` snapshot)

publishedIds :: MonadMetadata m => Pattern -> m [Identifier]
publishedIds = fmap (fmap fst . filter (isPublished . snd)) . getAllMetadata
