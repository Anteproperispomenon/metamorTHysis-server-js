{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.JS
  (

  ) where

import Data.ByteString qualified as BS

import Data.Kind qualified as Kind

import Language.Haskell.TH

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

generateJS :: forall (myApi :: Kind.Type). Q ()
generateJS = generateJS' "static/run_convert.js"

generateJS' :: forall (myApi :: Kind.Type). FilePath -> Q ()
generateJS' fp = do
  