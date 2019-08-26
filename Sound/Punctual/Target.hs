{-# LANGUAGE OverloadedStrings #-}

module Sound.Punctual.Target where

import Data.Text (Text)

import Sound.Punctual.Extent

data Target =
  Panned Extent |
  Splay |
  RGB |
  Red |
  Green |
  Blue |
  Alpha |
  NamedTarget Text deriving (Show,Eq)
