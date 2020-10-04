{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.Binary (Binary)
import Data.Bool (bool)
import Data.Foldable (fold, traverse_)
import Data.List (group, partition, sortOn)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.String (IsString)
import Data.Text (pack, replace, unpack)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Hakyll
import Network.HTTP.Base (urlEncode)
import System.Environment (getEnvironment)
import System.FilePath (replaceExtension)
import System.Random (RandomGen, newStdGen)
import System.Random.Shuffle (shuffle')
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Definition (Block (..), Inline (..))
import Text.Pandoc.Walk (walkM)

--------------------------------------------------------------------------------

root :: String
root = "https://odone.io"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "odone.io",
      feedDescription = "Rambling on software as a learning tool",
      feedAuthorName = "Riccardo Odone",
      feedAuthorEmail = "",
      feedRoot = root
    }

previewHost' :: String
previewHost' = previewHost defaultConfiguration

previewPort' :: Int
previewPort' = previewPort defaultConfiguration

previewUrl :: String
previewUrl = fold ["http://", previewHost', ":", show previewPort', "/"]

postsPattern :: Pattern
postsPattern = "posts/*"

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
          >>= loadAndApplyTemplate "templates/404-wrapper.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
    tags <- buildTags' postsPattern (fromCapture "tags/*.html")
    matchMetadata postsPattern isPublished $ do
      let livePath = (`replaceExtension` "html") . toFilePath
      route . customRoute $ livePath
      compile $ do
        meta <- getMetadata =<< getUnderlying
        let title = fromJust . lookupString "title" $ meta
        let description = fromJust . lookupString "description" $ meta
        href <- tweetLink . fold $ [title, "📒", description]
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/post.html" (constField "tweet" href <> postCtx env tags)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post-wrapper.html" randomPostsCtx
          >>= loadAndApplyTemplate "templates/default.html" (postCtx env tags)
          >>= relativizeUrls
    matchMetadata postsPattern (not . isPublished) $ do
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
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/post.html" (postCtx env tags)
          >>= loadAndApplyTemplate "templates/post-wrapper.html" randomPostsCtx
          >>= loadAndApplyTemplate "templates/default.html" (postCtx env tags)
          >>= relativizeUrls
          >>= (\x -> putDraftUrl x >> pure x)
    tagsRules tags $ \tag pattern_ -> do
      route idRoute
      compile $ archive env tags (Just tag) postsPattern pattern_
    create ["archive.html"] $ do
      route idRoute
      compile $ archive env tags Nothing postsPattern postsPattern
    create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
        pages <- loadAll (fromList ["archive.html"])
        posts <- recentFirst =<< loadAllPublished env postsPattern
        categoriesAndTags <- uncurry (<>) <$> getCategoriesAndTags posts
        let sitemapCtx =
              listField "pages" defaultContext (pure pages)
                <> listField "posts" (dateField "date" "%F" <> postCtx env tags) (pure posts)
                <> listField "tags" (tagsCtx Nothing) (traverse makeItem categoriesAndTags)
        makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = constField "title" "Riccardo Odone - Home" <> constField "index" "" <> defaultContext
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
            =<< traverse (loadAndApplyTemplate "templates/post-wrapper-atom.html" defaultContext)
            =<< loadAllSnapshotsPublished postsPattern "content"
        renderAtom feedConfiguration feedCtx posts

--------------------------------------------------------------------------------

type RandomPost = (String, String, String)

randomPostsCtx :: Context String
randomPostsCtx = listFieldWith "randomPosts" randomPostCtx randomPosts <> defaultContext

randomPostCtx :: Context RandomPost
randomPostCtx =
  field "title" (pure . fst' . itemBody)
    <> field "description" (pure . snd' . itemBody)
    <> field "url" (pure . trd' . itemBody)

randomPosts :: Item a -> Compiler [Item RandomPost]
randomPosts item = do
  gen <- unsafeCompiler newStdGen
  ids <- filter (itemIdentifier item /=) <$> publishedIds postsPattern
  let randomIds = take 3 $ shuffle' ids (length ids) gen
  metas <- traverse getMetadata randomIds
  routes <- traverse (fmap (toUrl . fromJust) . getRoute) randomIds
  let toRandomPost (meta, ident) = (,,) <$> lookupString "title" meta <*> lookupString "description" meta <*> Just ident
  let randoms = fromJust . toRandomPost <$> zip metas routes
  traverse makeItem randoms

type Tag = (Int, Char, String)

fst' (a, _, _) = a

snd' (_, b, _) = b

trd' (_, _, c) = c

getCategoriesAndTags :: (MonadMetadata m, MonadFail m) => [Item String] -> m ([Tag], [Tag])
getCategoriesAndTags posts = do
  let identifiers = fmap itemIdentifier posts
  tags <- traverse getTags' identifiers
  -- tags -> [[a, b], [d, e], [a, c]]
  -- withLabel -> ([(♕,a), (♔,d), (♕,a)], [(♕,b), (♔,e), (♕,c)])
  -- withLengthAndLabel -> ([(1,♔,d), (2,♕,a)], [(1,♔,e), (1,♕,b), (1,♕,c)])
  pure . withLengthAndLabel . withLabel $ tags
  where
    -- ♔ ♕ ♖ ♗ ♘ ♙
    toLabel "Functional Programming" = '♕'
    toLabel "Essential Skills" = '♔'
    bimap' :: Bifunctor p => (a -> b) -> p a a -> p b b
    bimap' f = bimap f f
    withCategory :: [String] -> [(String, String)]
    withCategory ts@(t : _) = zip (repeat t) ts
    withLabel :: [[String]] -> ([(Char, String)], [(Char, String)])
    withLabel = bimap' (fmap (first toLabel)) . partition (uncurry (==)) . concatMap withCategory
    withLengthAndLabel = bimap' (fmap (\xs -> (length xs, fst . head $ xs, snd . head $ xs)) . group . sortOn snd)

archive :: [(String, String)] -> Tags -> Maybe String -> Pattern -> Pattern -> Compiler (Item String)
archive env allTags mSelectedTag allPattern filterPattern = do
  allPosts <- recentFirst =<< loadAllPublished env allPattern
  (categories, tags) <- getCategoriesAndTags allPosts
  let filteredPosts = filter (matches filterPattern . itemIdentifier) allPosts
  let archiveCtx =
        listField "tags" (tagsCtx mSelectedTag) (traverse makeItem tags)
          <> listField "categories" (tagsCtx mSelectedTag) (traverse makeItem categories)
          <> listField "posts" (postCtx env allTags) (pure filteredPosts)
          <> constField "title" "Riccardo Odone - Archives"
          <> defaultContext
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    >>= relativizeUrls

tagsCtx :: Maybe String -> Context Tag
tagsCtx mSelectedTag =
  field "url" (pure . toUrl . tagUrl . trd' . itemBody)
    <> field "status" (pure . bool "unselected" "selected" . (==) mSelectedTag . Just . trd' . itemBody)
    <> field "tag" (pure . trd' . itemBody)
    <> field "icon" (pure . (: []) . snd' . itemBody)
    <> field "count" (pure . show . fst' . itemBody)
  where
    tagUrl tag = if Just tag == mSelectedTag then "/archive.html" else "/tags/" <> tag <> ".html"

postCtx :: [(String, String)] -> Tags -> Context String
postCtx env tags =
  listFieldWith "seriesPosts" seriesPostsCtx (seriesPosts env)
    <> tagsField' "tags" tags
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

type Serie = (String, String, String)

seriesPostsCtx :: Context Serie
seriesPostsCtx =
  field "title" (pure . fst' . itemBody)
    <> field "url" (pure . snd' . itemBody)
    <> field "klass" (pure . trd' . itemBody)

seriesPosts :: [(String, String)] -> Item a -> Compiler [Item Serie]
seriesPosts env item = do
  seriesMeta <- fmap (lookupString "series") . getMetadata . itemIdentifier $ item
  ids <- if isDevelopmentEnv env then allIds postsPattern else publishedIds postsPattern
  metas <- traverse getMetadata ids
  routes <- traverse (fmap (toUrl . fromJust) . getRoute) ids
  let toSeriesTriple (meta, route, id_) =
        (,,,) <$> lookupString "series" meta <*> lookupString "title" meta <*> Just route <*> Just id_
  let matchingSeries series (s, _, _, _) = Just s == series
  let toSeriesTuple (_, title, route, id_) =
        (title, route, if id_ == itemIdentifier item then "selected" else "unselected")
  traverse makeItem
    . fmap toSeriesTuple
    . filter (matchingSeries seriesMeta)
    . mapMaybe toSeriesTriple
    $ zip3 metas routes ids

loadAllPublished :: (Binary a, Typeable a) => [(String, String)] -> Pattern -> Compiler [Item a]
loadAllPublished env pattern_ = if isDevelopmentEnv env then all else published
  where
    all = loadAll pattern_
    published = publishedIds pattern_ >>= traverse load

isDevelopmentEnv :: [(String, String)] -> Bool
isDevelopmentEnv env = lookup "HAKYLL_ENV" env == Just "development"

loadAllSnapshotsPublished :: (Binary a, Typeable a) => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshotsPublished pattern_ snapshot = publishedIds pattern_ >>= traverse (`loadSnapshot` snapshot)

allIds :: MonadMetadata m => Pattern -> m [Identifier]
allIds = fmap (fmap fst) . getAllMetadata

publishedIds :: MonadMetadata m => Pattern -> m [Identifier]
publishedIds = fmap (fmap fst . filter (isPublished . snd)) . getAllMetadata

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWith getTags' simpleRenderLink' mconcat

simpleRenderLink' :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink' _ Nothing = Nothing
simpleRenderLink' tag (Just filePath) =
  Just $
    H.a
      ! A.href (toValue $ toUrl filePath)
      ! A.class_ "btn btn-tag-unselected btn-sm"
      $ toHtml tag

buildTags' :: MonadMetadata m => Pattern -> (String -> Identifier) -> m Tags
buildTags' = buildTagsWith getTags'

getTags' :: MonadMetadata m => Identifier -> m [String]
getTags' identifier = do
  metadata <- getMetadata identifier
  pure $
    fromMaybe ["_untagged_"] $
      lookupStringList "tags" metadata <|> (map trim . splitAll "," <$> lookupString "tags" metadata)

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions (walkM transform)
  where
    transform :: Block -> Compiler Block
    transform (CodeBlock (_, ["pullquote"], []) content) = do
      href <- pack <$> tweetLink (unpack content)
      pure $
        RawBlock "html" $
          fold
            [ "<blockquote class=\"pullquote\">",
              "<span>",
              content,
              "</span>",
              "<a target=\"_blank\" rel=\"noopener\" href=",
              href,
              ">",
              twitterIcon,
              "</a>",
              "</blockquote>"
            ]
    transform x = pure x

tweetLink :: String -> Compiler String
tweetLink content = do
  identifier <- getUnderlying
  meta <- getMetadata identifier
  maybeRoute <- getRoute identifier
  let toTwitterTag = ("#" <>) . unpack . replace " " "" . pack
  let tags = unwords . fmap toTwitterTag . fromMaybe [] . lookupStringList "tags" $ meta
  let url = ((root <> "/") <>) . fromJust $ maybeRoute
  let text = urlEncode . fold $ [content, " ", "via @RiccardoOdone", "\n\n", tags, "\n\n", url]
  let query = fold ["text", "=", text]
  let href = "https://twitter.com/intent/tweet?" <> query
  pure href

twitterIcon :: IsString a => a
twitterIcon = "<svg class=\"tweet-this-icon\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 400 400\"><defs><style>.cls-1{fill:none;}.cls-2{fill:#f76ca5;}</style></defs><title>Twitter_Logo_Blue</title><rect class=\"cls-1\" width=\"400\" height =\"400\"/><path class=\"cls-2\" d=\"M153.62,301.59c94.34,0,145.94-78.16,145.94-145.94,0-2.22,0-4.43-.15-6.63A104.36,104.36,0,0,0,325,122.47a102.38,102.38,0,0,1-29.46,8.07,51.47,51.47,0,0,0,22.55-28.37,102.79,102.79,0,0,1-32.57,12.45,51.34,51.34,0,0,0-87.41,46.78A145.62,145.62,0,0,1,92.4,107.81a51.33,51.33,0,0,0,15.88,68.47A50.91,50.91,0,0,1,85,169.86c0,.21,0,.43,0,.65a51.31,51.31,0,0,0,41.15,50.28,51.21,51.21,0,0,1-23.16.88,51.35,51.35,0,0,0,47.92,35.62,102.92,102.92,0,0,1-63.7,22A104.41,104.41,0,0,1,75,278.55a145.21,145.21,0,0,0,78.62,23\"/></svg>"
