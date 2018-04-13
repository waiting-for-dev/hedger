{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Hedger.Migration
    ( migrate
      , dBPath
      , CategoriesSchema
      , ExpensesSchema
      , categories
      , withDB
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
  , SeldaM
  )
import Database.Selda.SQLite (withSQLite)
import System.Directory
  (
  getXdgDirectory
  ,  XdgDirectory( XdgData )
  , createDirectoryIfMissing
  )

type CategoriesSchema = RowID:*:Text
type ExpensesSchema = RowID:*:Text:*:Double:*:RowID

dBDir :: IO FilePath
dBDir = getXdgDirectory XdgData "hedger"

dBFilename :: String
dBFilename = "hedger.sqlite"

dBPath :: IO FilePath
dBPath = do
    dir <- dBDir
    return $ dir </> "hedger.sqlite"

withDB :: SeldaM a -> IO a
withDB act = do
  path <- dBPath
  withSQLite path act

migrate :: IO ()
migrate = do
    dir <- dBDir
    createDirectoryIfMissing True dir
    -- forM_ (categories, expenses)
    --    $ withDB . createTable
    withDB . tryCreateTable $ categories
    withDB . tryCreateTable $ expenses

categories:: Table (CategoriesSchema)
(categories, categoryID :*: rest) = tableWithSelectors "categories" 
    $ autoPrimary "id"
    :*: required "name"

expenses:: Table (ExpensesSchema)
expenses = table "expenses" 
    $ autoPrimary "id"
    :*: required "name"
    :*: required "amount"
    :*: required "category_id" `fk` (categories, categoryID)
