{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- hmm...

module Metamorth.Server.JS
  ( generateJS
  , generateJs
  , generateJsAt
  ) where

import Control.Monad

import Data.ByteString qualified as BS

import Data.Kind qualified as Kind

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Servant.JS
import Servant.JS.Vanilla

import Data.Proxy

import Servant.Foreign qualified as SF

import System.OsPath      qualified as OS
import System.File.OsPath qualified as OF

import System.Directory.OsPath

type ToJS :: Kind.Type -> Kind.Constraint
type ToJS myApi = (SF.HasForeign SF.NoTypes SF.NoContent myApi, SF.GenerateList SF.NoContent (SF.Foreign SF.NoContent myApi))

-- type ToJS1 :: Kind.Type -> Kind.Constraint
-- type ToJS1 myApi = SF.HasForeign SF.NoTypes SF.NoContent myApi

-- type ToJS2 :: Kind.Type -> Kind.Constraint
-- type ToJS2 myApi = SF.GenerateList SF.NoContent (SF.Foreign SF.NoContent myApi)

-- | Generate JavaScript code for a specific
--   API, and store it at "static/run_convert.js".
--   To use this function, make sure you have
--   TypeApplications enabled, and then write
--   
--     > generateJS @MyAPI
--
--   ...where @MyAPI@ is the name of the type 
--   of your API.
generateJS :: forall (myApi :: Kind.Type). (ToJS myApi) => Q [Dec]
generateJS = generateJs @myApi

-- | Synonym for `generateJS`.
generateJs :: forall (myApi :: Kind.Type). (ToJS myApi) => Q [Dec]
generateJs = generateJsAt @myApi "static/run_convert.js"

-- | Like `generateJS`, but allows you to specify
--   where you want to place the resulting JS file.
--   Be careful; this function will automatically
--   overwrite the file in question.
generateJsAt :: forall (myApi :: Kind.Type). (ToJS myApi) => FilePath -> Q [Dec]
generateJsAt fp = generateJsAt' @myApi fp (Proxy :: Proxy myApi)

generateJsAt' :: forall (myApi :: Kind.Type). (ToJS myApi) => FilePath -> Proxy myApi -> Q [Dec]
generateJsAt' fp prox = do
  let myText = jsForAPI prox vanillaJS
      myBS   = TE.encodeUtf8 myText
  runIO $ checkAndOverwrite myBS fp
  return []

-- | Overwrite a file if the contents don't match.
checkAndOverwrite :: BS.ByteString -> FilePath -> IO ()
checkAndOverwrite bs fp' = do
  fp  <- OS.encodeUtf fp'
  bl1 <- doesPathExist fp
  bl2 <- doesFileExist fp
  when (bl1 && (not bl2)) $ fail $
    "Couldn't write to path \"" ++ fp' ++ "\"; the path is a directory, not a file."

  if (not bl1) 
    then do
      -- Make the directory if it doesn't exist.
      let dir = OS.takeDirectory fp
      createDirectoryIfMissing True dir
      -- Write the actual file.
      OF.writeFile' fp bs
    else do
      -- Check if the file matches (probably won't...)
      -- Doesn't actually do this anymore, since it
      -- could potentially be VERY inefficient if
      -- overwriting a huge file. Maybe if there were
      -- a way to only read in the first n bytes of
      -- a file.
      -- bs' <- OF.readFile fp
      -- if (bs' == bs)
      --   then return ()
      --   else do
      OF.writeFile' fp bs

          



