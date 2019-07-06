--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative ( empty )
import           Data.List.Safe ( find, groupBy, head, intersperse
                                , isPrefixOf, sortBy, stripPrefix )
import           Data.List.Split ( splitOn )
import           Data.Semigroup ( (<>) )
import           GHC.Exts ( fromString )
import           Hakyll
import           Prelude hiding ( head )
import           System.FilePath ( takeDirectory, takeBaseName, (</>) )
import           Text.CSL.Input.Bibtex ( readBibtex )
import           Text.CSL ( Reference )
import           Text.CSL.Reference ( author, containerTitle, collectionTitle
                                    , issued, note, refId, title, unLiteral
                                    , year )
import           Text.CSL.Style ( familyName, givenName, unFormatted
                                , writeCSLString )
import           Text.Pandoc.Definition ( Inline ( LineBreak ) )

-- Rules -----------------------------------------------------------------------

main :: IO ()
main = do
  biblio <- readBibtex' "bibliography.bib"
  hakyll $ do
      match ("CNAME" .||. "img/**" .||. "static/**") $ do
          route   idRoute
          compile copyFileCompiler

      match "css/*" $ do
          route   idRoute
          compile compressCssCompiler

      matchList ["about.md", "projects.md"] $ do
          route   $ dirRoute
          compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

      match "publications.md" $ do
          route   $ dirRoute
          compile $ do
              let papers = filter (("Sam L. Thomas" `elem`) . bibAuthor) biblio
                  pubCtx = bibByYearCtx papers
                        <> constField "title" "Publications"
                        <> defaultContext

              getResourceBody
                  >>= applyAsTemplate pubCtx
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/default.html" pubCtx
                  >>= relativizeUrls

      match "blog/**.md" $ do
          route   $ setExtension "html"
          compile $ pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    postListCtx
              >>= loadAndApplyTemplate "templates/default.html" postListCtx
              >>= relativizeUrls

      match "blog.md" $ do
          route   $ dirRoute
          compile $ do
              posts <- recentFirst =<< loadAll "blog/**.md"
              let writingCtx = listField "posts" postCtx (pure posts)
                            <> constField "title" "Blog"
                            <> defaultContext

              getResourceBody
                  >>= applyAsTemplate writingCtx
                  >>= renderPandoc
                  >>= loadAndApplyTemplate "templates/default.html" writingCtx
                  >>= relativizeUrls

      match "index.md" $ do
          route   $ setExtension "html"
          compile $ pandocCompiler
                  >>= loadAndApplyTemplate "templates/home.html" defaultContext
                  >>= relativizeUrls

      create ["404.html"] $ do
          route   $ idRoute
          compile $ do
              let ctx = constField "title" "404"
                     <> constField "body"  "Resource not found."
                     <> defaultContext

              makeItem "" >>= loadAndApplyTemplate "templates/default.html" ctx

      match "templates/*" $ compile templateBodyCompiler

-- Contexts --------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d" <> defaultContext

postListCtx :: Context String
postListCtx =
  dateField "date" "%B %e, %Y" <> defaultContext

-- Routes ----------------------------------------------------------------------

dirRoute :: Routes
dirRoute = customRoute $ genRoute . toFilePath
  where genRoute path =
          takeDirectory path </> takeBaseName path </> "index.html"

-- Utility ---------------------------------------------------------------------

matchList :: [Identifier] -> Rules () -> Rules ()
matchList = match . fromList

readBibtex' :: FilePath -> IO [Reference]
readBibtex' = readBibtex (const True) True False

-- Bibliography ----------------------------------------------------------------

bibId :: Reference -> String
bibId = unLiteral . refId

-- TODO: how will this handle a name with von components?
bibAuthor :: Reference -> [String]
bibAuthor = fmap format . author
  where lastName agent =
          writeCSLString . unFormatted . familyName $ agent
        firstNames agent =
          map (writeCSLString . unFormatted) $ givenName agent
        format agent =
          concat . intersperse " " $ firstNames agent ++ [lastName agent]

bibTitle :: Reference -> String
bibTitle = writeCSLString . unFormatted . title

bibConference :: Reference -> String
bibConference = writeCSLString . unFormatted . containerTitle

bibSeries :: Reference -> String
bibSeries = writeCSLString . unFormatted . collectionTitle

bibYear :: Reference -> Maybe Int
bibYear = (year =<<) . head . issued

bibNote :: Reference -> [String]
bibNote = map writeCSLString . splitOn [LineBreak] . unFormatted . note

bibNoteWithPrefix :: String -> Reference -> Maybe String
bibNoteWithPrefix k = (stripPrefix k =<<) . find (isPrefixOf k) . bibNote

bibNotePdf :: Reference -> Maybe String
bibNotePdf = bibNoteWithPrefix "pdf: "

bibNoteSlides :: Reference -> Maybe String
bibNoteSlides = bibNoteWithPrefix "slides: "

bibNoteAR :: Reference -> Maybe String
bibNoteAR = bibNoteWithPrefix "acceptance rate: "

bibField :: (Reference -> a) -> Item Reference -> Compiler a
bibField f = pure . f . itemBody

bibCtx :: Context Reference
bibCtx =
    field "title" (bibField bibTitle) <>
    field "conference" (bibField bibConference) <>
    maybeField "pdf" bibNotePdf <>
    maybeField "slides" bibNoteSlides <>
    maybeField "arate" bibNoteAR <>
    field "author" authorsField <>
    listFieldWith "authors" authorCtx (mapM makeItem . bibAuthor . itemBody)
  where highlight who =
          fmap (\who' -> if who == who' then "<strong>" ++ who' ++ "</strong>"
                         else who')
        joinAuthors xs@(_ : _ : _) =
          (concat . intersperse ", " . init) xs ++ " and " ++ last xs
        joinAuthors [x] = x
        authorsField =
          fmap (joinAuthors . highlight "Sam L. Thomas") . bibField bibAuthor

maybeField :: String -> (a -> Maybe String) -> Context a
maybeField k f = Context g
    where g k' [] item = if k == k'
                            then case f . itemBody $ item of
                                   Just v  -> return $ StringField v
                                   Nothing -> empty
                            else empty

bibByCat :: String -> String -> [Reference] -> Context String
bibByCat cat v rs =
    constField cat v <>
    listField "publications" bibCtx (mapM makeItem rs)

bibByYearCtx :: [Reference] -> Context String
bibByYearCtx rs =
    listField "publications" bibYearCtx (mapM (makeItem . yearAndBibs) $ sortedRefs rs)
        where sameYear r r'      = bibYear r == bibYear r'
              printYear (Just y) = show y
              printYear Nothing  = "Others"
              getYear (g : _)    = bibYear g
              yearAndBibs g      = (printYear . getYear $ g, g)
              sortedRefs         =
                sortBy (\u v -> compare (getYear v) (getYear u)) . groupBy sameYear

bibYearCtx :: Context (String, [Reference])
bibYearCtx = Context f
    where f "year"         [] item = return $ StringField (fst . itemBody $ item)
          f "publications" [] item = return $ ListField bibCtx (fmap bibItem . snd . itemBody $ item)
          f _        _  _    = empty

authorCtx :: Context String
authorCtx = Context f
    where f "author" [] item = return $ StringField (itemBody item)
          f _        _  _    = empty

bibItem :: Reference -> Item Reference
bibItem r =
  Item {
      itemIdentifier = fromString $ bibId r
    , itemBody       = r
  }
