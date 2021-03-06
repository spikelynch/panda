{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3
    ( BootstrapFormLayout (..), renderBootstrap3, withSmallInput )

import System.FilePath
import System.Directory (doesFileExist)
import qualified System.IO.UTF8 as IU
import qualified Data.ByteString.Lazy.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as BL
import qualified Crypto.Hash.SHA1 as SHA
import Text.Pandoc
import Text.Pandoc.MediaBag

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
    defaultLayout $ do
        setTitle "Welcome to Panda!"
        $(widgetFile "homepage")



postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    case result of
     FormSuccess (file, info) -> do
       ( filename, hash, format ) <- writeToServer file
       _ <- runDB $ insert (Document (T.pack filename) (E.decodeASCII hash) format)
       setMessage "Document uploaded"
     _ -> do
       setMessage "Something went wrong"

    defaultLayout $ do
        setTitle "Upload!"
        $(widgetFile "homepage")



sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "Convert to") Nothing





--writeToServer :: FileInfo -> Handler ( FilePath, BL.ByteString, Text )
writeToServer file = do
    let filename = T.unpack $ fileName file
        path = mkFilePath filename
    liftIO $ fileMove file path
    bytes <- liftIO $ BL.readFile path
    hash <- return $ SHA.hashlazy bytes
    format <- return $ fileContentType file
    return ( filename, hash, format )

mkFilePath :: String -> FilePath
mkFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "static"


getSelectR :: FilePath -> Handler Html
getSelectR fp = do
  let file = mkFilePath fp
  found <- liftIO $ doesFileExist file
  case found of
   False -> notFound
   True -> defaultLayout $ do
     setTitle "File page"
     $(widgetFile "file")


-- 

getRenderR :: FilePath -> Handler TypedContent
getRenderR fp = do
  bytes <- liftIO $ BL.readFile $ mkFilePath fp
  ( pDoc, media ) <- return $ readDocx def bytes
  selectRep $ do
    provideRep $ do
      return $ writeHtml def pDoc
      


