{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hedger.Migration
    (
      migrate
    ) where

import Data.Foldable (forM_)
import System.FilePath ((</>))

import Database.Selda
  (
  Table
  , RowID
  , Text
  , table
  , tableWithSelectors
  , (:*:)(..)
  , RowID
  , tryCreateTable
  , autoPrimary
  , required
  , fk
  )
import Database.Selda.SQLite (withSQLite)
import System.Directory
  (
  getXdgDirectory
  ,  XdgDirectory( XdgData )
  , createDirectoryIfMissing
  )

dBPath :: IO FilePath
dBPath = do
    configDir <- getXdgDirectory XdgData "hedger"
    return $ configDir </> "hedger.sqlite"
-- dBPath = (</>) <$> configPath <*> dBFilename
--   where
--     configPath = getXdgDirectory XdgData ""
--     dBFilename = return "hedger.sqlite"

migrate :: IO ()
migrate = do
    path <- dBPath
    createDirectoryIfMissing True path
    forM_ (categories, expenses)
      $ withSQLite path . tryCreateTable

categories:: Table (RowID:*:Text)
(categories, categoryID :*: rest) = tableWithSelectors "categories" 
    $ autoPrimary "id"
    :*: required "name"

expenses:: Table (RowID:*:Text:*:Double:*:RowID)
expenses = table "expenses" 
    $ autoPrimary "id"
    :*: required "name"
    :*: required "amount"
    :*: required "category_id" `fk` (categories, categoryID)
