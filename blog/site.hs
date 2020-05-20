{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Binary (Binary)
import Data.Bool (bool)
import Data.Foldable (fold, traverse_)
import Data.List (intersperse, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Hakyll
import System.Environment (getEnvironment)
import System.FilePath (replaceExtension)
import Text.Blaze.Html ((!), toHtml, toValue)
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
    match "404.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
    tags <- buildTags' "posts/*" (fromCapture "tags/*.html")
    matchMetadata "posts/*" isPublished $ do
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
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
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
          >>= relativizeUrls
          >>= (\x -> putDraftUrl x >> pure x)
    tagsRules tags $ \tag pattern_ -> do
      route idRoute
      compile $ archive env tags (Just tag) "posts/*" pattern_
    create ["archive.html"] $ do
      route idRoute
      compile $ archive env tags Nothing "posts/*" "posts/*"
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = constField "title" "Home" <> defaultContext
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

archive :: [(String, String)] -> Tags -> Maybe String -> Pattern -> Pattern -> Compiler (Item String)
archive env tags mSelectedTag allPattern filterPattern = do
  allPosts <- recentFirst =<< loadAllPublished env allPattern
  allTags <- fmap (sort . nub . concat) . traverse getTags' . fmap itemIdentifier $ allPosts
  let tagsCtx =
        field "url" (pure . (\tag -> if Just tag == mSelectedTag then "/archive.html" else "/tags/" <> tag <> ".html") . itemBody)
          <> field "status" (pure . bool "unselected" "selected" . (==) mSelectedTag . Just . itemBody)
          <> field "tag" (pure . itemBody)
  let filteredPosts = filter (matches filterPattern . itemIdentifier) allPosts
  let archiveCtx =
        listField "tags" tagsCtx (traverse makeItem allTags)
          <> listField "posts" (postCtx tags) (pure filteredPosts)
          <> constField "title" "Archives"
          <> defaultContext
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    >>= relativizeUrls

postCtx :: Tags -> Context String
postCtx tags = tagsField' "tags" tags <> dateField "date" "%B %e, %Y" <> defaultContext

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

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWith getTags' simpleRenderLink' mconcat

simpleRenderLink' :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink' _ Nothing = Nothing
simpleRenderLink' tag (Just filePath) =
  Just
    $ H.a
      ! A.href (toValue $ toUrl filePath)
      ! A.class_ "btn btn-tag-unselected btn-sm"
    $ toHtml tag

buildTags' :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildTags' = buildTagsWith getTags'

getTags' :: MonadMetadata m => Identifier -> m [String]
getTags' identifier = do
  metadata <- getMetadata identifier
  pure $ fromMaybe ["_untagged_"] $
    lookupStringList "tags" metadata <|> (map trim . splitAll "," <$> lookupString "tags" metadata)
