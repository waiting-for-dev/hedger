{-# LANGUAGE OverloadedStrings #-}

module Hedger.Category
  (
    listCategories
    , addCategory
  ) where

import Database.Selda
  (
    SeldaM(..)
    , query
    , select
    , insert_
    , (:*:)(..)
    , def
    , Text
    , liftIO
  )

import Hedger.Migration
  (
    withDB
  , CategoriesSchema
  , categories
  )

listCategories :: IO [CategoriesSchema]
listCategories = withDB $ query
                   $ select categories

addCategory :: Text -> IO ()
addCategory name = withDB $ insert_ categories 
                     [ def :*: name ]
