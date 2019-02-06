-- https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/extending_ghc.html#source-plugins
-- https://downloads.haskell.org/~ghc/8.6.3/docs/html/libraries/ghc-8.6.3/GhcPlugins.html
module Caltrop ( plugin ) where

import qualified Bag
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified ErrUtils
import qualified GhcPlugins
import qualified HsSyn
import qualified Lexer
import qualified MonadUtils
import qualified Parser
import qualified StringBuffer

plugin :: GhcPlugins.Plugin
plugin = GhcPlugins.defaultPlugin
  { GhcPlugins.parsedResultAction = action
  , GhcPlugins.pluginRecompile = GhcPlugins.purePlugin
  }

action
  :: [GhcPlugins.CommandLineOption]
  -> GhcPlugins.ModSummary
  -> GhcPlugins.HsParsedModule
  -> GhcPlugins.Hsc GhcPlugins.HsParsedModule
action options summary module_ = do
  caltrop options summary module_
  pure module_

caltrop
  :: [GhcPlugins.CommandLineOption]
  -- ^ A plain old list of strings like you might get from
  -- 'System.Environment.getArgs'.
  -> GhcPlugins.ModSummary
  -- ^ Include the date that the file was saved, the module name, and the
  -- imported modules. Unfortunately it doesn't seem to include any information
  -- about the imports other than the module names. You can't tell if something
  -- was imported qualified, for example.
  -> GhcPlugins.HsParsedModule
  -- ^ All of the interesting stuff. Supposedly contains the original source as
  -- well as the parsed tokens, but documentation is scarce.
  -> GhcPlugins.Hsc ()
caltrop _ modSummary hsParsedModule = do
  dynFlags <- GhcPlugins.getDynFlags
  let
    srcSpan
      = maybe GhcPlugins.noSrcSpan
        (GhcPlugins.mkGeneralSrcSpan . GhcPlugins.mkFastString)
      . GhcPlugins.ml_hs_file
      $ GhcPlugins.ms_location modSummary

  -- TODO: Like imports, this should check that groups are sorted. But even
  -- then, does it make sense to require this? Libraries probably never want
  -- this, but applications might.
  let
    rdrNames
      = map (HsSyn.ieName . GhcPlugins.unLoc)
      . maybe [] GhcPlugins.unLoc
      . HsSyn.hsmodExports
      . GhcPlugins.unLoc
      $ GhcPlugins.hpm_module hsParsedModule
  Monad.unless (rdrNames == List.sort rdrNames)
    . MonadUtils.liftIO
    . GhcPlugins.printOrThrowWarnings dynFlags
    . Bag.unitBag
    . ErrUtils.mkPlainWarnMsg dynFlags
      -- TODO: Can this point to the specific export that isn't sorted?
      ( maybe srcSpan GhcPlugins.getLoc
      . HsSyn.hsmodExports
      . GhcPlugins.unLoc
      $ GhcPlugins.hpm_module hsParsedModule
      )
    $ GhcPlugins.text "exports not sorted"

  -- TODO: This should really check that each *group* of imports (separated by
  -- blank lines) is sorted, not that all of the imports are sorted.
  let
    moduleNames
      = map (GhcPlugins.unLoc . HsSyn.ideclName . GhcPlugins.unLoc)
      . HsSyn.hsmodImports
      . GhcPlugins.unLoc
      $ GhcPlugins.hpm_module hsParsedModule
  Monad.unless (moduleNames == List.sort moduleNames)
    . MonadUtils.liftIO
    . GhcPlugins.printOrThrowWarnings dynFlags
    . Bag.unitBag
    . ErrUtils.mkPlainWarnMsg dynFlags
      -- TODO: Can this point to the specific import that isn't sorted?
      ( case map GhcPlugins.getLoc . HsSyn.hsmodImports . GhcPlugins.unLoc $ GhcPlugins.hpm_module hsParsedModule of
        [] -> srcSpan
        x : xs -> foldr GhcPlugins.combineSrcSpans x xs
      )
    $ GhcPlugins.text "imports not sorted"

  -- This shows how to use the GHC library to parse an expression. I would like
  -- to be able to use arbitrary expressions as suggestions like HLint. (For
  -- example, `\ x -> x` should be `id`.) However doing so will require being
  -- able to tell if two expressions are the same other than their variable
  -- names. <https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence>
  let
    result
      = Lexer.unP Parser.parseExpression
      . Lexer.mkPState dynFlags (StringBuffer.stringToStringBuffer "\\ x -> x")
      $ GhcPlugins.mkRealSrcLoc (GhcPlugins.mkFastString "") 0 0
  case result of
    Lexer.POk _ expression -> MonadUtils.liftIO
      . putStrLn
      . GhcPlugins.showSDocUnsafe
      $ GhcPlugins.ppr expression
    _ -> MonadUtils.liftIO $ putStrLn "Failed to parse expression :("
