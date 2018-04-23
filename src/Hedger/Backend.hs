{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}

module Hedger.Backend
  (
    list
  ) where

import Database.Selda
  (
    Result
    , Columns
    , Res
    , Table
    , query
    , select
    , Cols
  )

import Hedger.Migration
  (
    withDB
  )

-- list :: (Result (Cols s a), Columns (Cols s a))  => Table a -> IO [Res (Cols s a)]
list table = withDB $ query (select table)
