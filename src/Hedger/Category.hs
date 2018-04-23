{-# LANGUAGE OverloadedStrings #-}

module Hedger.Category
  (
    listCategories
    , addCategory
    , list
  ) where

import Database.Selda
  (
    SeldaM(..)
    , insert_
    , (:*:)(..)
    , def
    , Text
  )

import Hedger.Backend (list)

import Hedger.Migration
  (
  CategoriesSchema
  , categories
  , withDB
  )

listCategories :: IO [CategoriesSchema]
listCategories = list categories

addCategory :: Text -> IO ()
addCategory name = withDB $ insert_ categories 
                     [ def :*: name ]
