{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Hedger.Migration
    (
      migrate
      , dBPath
      , CategoriesSchema
      , ExpensesSchema
      , categories
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

migrate :: IO ()
migrate = do
    dir <- dBDir
    createDirectoryIfMissing True dir
    path <- dBPath
    forM_ (categories, expenses)
      $ withSQLite path . tryCreateTable

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
