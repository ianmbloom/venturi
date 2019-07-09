module Language.Venturi.Generate
  ( generateCode
  )
where

import qualified Language.Venturi.Core.ToVarLc as ToVarLc
import qualified Language.Venturi.Calculus.ReduceRecursive as Rr
import qualified Language.Venturi.Calculus.Reduce as R
import qualified Language.Venturi.Calculus.ApplyDataConTys as DC
--import qualified Language.Venturi.Calculus.ToCommand as C
import qualified Language.Venturi.Calculus.KnownCase as K
import qualified Language.Venturi.Compile.Pseudo as Ps
import Language.Venturi.Debug
import Language.Venturi.Core.Pretty

import GHC hiding (pprExpr)
import CoreMonad
import CoreSyn
import CoreToStg
import Var
import VarEnv
import DynFlags
import Outputable
import GHC.Paths(libdir)
import DynFlags(defaultLogAction)
import System.Directory
import System.FilePath.Posix
import Data.Data
import Data.List
import Data.Maybe (fromJust)

import Control.DeepSeq
import Control.Exception


varToString :: Var -> String
varToString = showSDocUnsafe . ppr

findNameInCoreBnds :: String
                   -> [(Var, Expr Var)]
                   -> Maybe Var
findNameInCoreBnds name = fmap fst . find ((==) name . varToString . fst)

findExpr :: CoreModule
         -> String
         -> Maybe (Expr Var)
findExpr coreModule name =
   fmap Var . findNameInCoreBnds name . flattenBinds . cm_binds $ coreModule

putStep title p term =
  do liftIO . putStrLn $ title
     liftIO . putStrLn . showSDocUnsafe . p . fromJust $ term

{-
compileToStg simplify fn = do
   -- First, set the target to the desired filename
   target <- guessTarget fn Nothing
   addTarget target
   _ <- load LoadAllTargets
   -- Then find dependencies
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) (mgModSummaries modGraph) of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
       (tcg, mod_guts) <- -- TODO: space leaky: call hsc* directly?
         do tm <- typecheckModule =<< parseModule modSummary
            let tcg = fst (tm_internals tm)
            (,) tcg . coreModule <$> desugarModule tm
       liftM (gutsToCoreModule (mg_safe_haskell mod_guts)) $
         if simplify
          then do
             -- If simplify is true: simplify (hscSimplify), then tidy
             -- (tidyProgram).
             hsc_env <- getSession
             simpl_guts <- liftIO $ do
               plugins <- readIORef (tcg_th_coreplugins tcg)
               hscSimplify hsc_env plugins mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts
     Nothing -> panic "compileToCoreModule: target FilePath not found in module dependency graph"
  where -- two versions, based on whether we simplify (thus run tidyProgram,
        -- which returns a (CgGuts, ModDetails) pair, or not (in which case
        -- we just have a ModGuts.
        gutsToCoreModule :: SafeHaskellMode
                         -> Either (CgGuts, ModDetails) ModGuts
                         -> CoreModule
        gutsToCoreModule safe_mode (Left (cg, md)) = CoreModule {
          cm_module = cg_module cg,
          cm_types  = md_types md,
          cm_binds  = cg_binds cg,
          cm_safe   = safe_mode
        }
        gutsToCoreModule safe_mode (Right mg) = CoreModule {
          cm_module  = mg_module mg,
          cm_types   = typeEnvFromEntities (bindersOfBinds (mg_binds mg))
                                           (mg_tcs mg)
                                           (mg_fam_insts mg),
          cm_binds   = mg_binds mg,
          cm_safe    = safe_mode
         }
-}


generateCode :: FilePath -> String -> IO () --(Maybe Ps.PseudoProgram)
generateCode source functionName =
  do
  cwd <- getCurrentDirectory
  runGhc (Just libdir) $
      do dynFlags <- getSessionDynFlags
         let dynFlagsMod =  updOptLevel 3 $
                            foldl gopt_set dynFlags [ Opt_SpecialiseAggressively
                                                    , Opt_ExposeAllUnfoldings
                                                    ]
         setSessionDynFlags dynFlagsMod
         liftIO $ putStrLn "venturi: Compiling Haskell Syntax to Haskell Core"
         coreModule <- compileToCoreSimplified (combine cwd source)
         liftIO $ putStrLn $ "venturi: Expanding coreToStg Expression " ++ functionName
         liftIO $ putStrLn $ showPpr dynFlags (cm_binds coreModule)
         let (stg,ccs) = coreToStg dynFlagsMod (cm_module coreModule) (cm_binds coreModule)
         liftIO $ putStrLn "after"
         liftIO $ putStrLn $ showPpr dynFlags stg
         return ()
         {-
         let term = findExpr coreModule functionName
         let env = ToVarLc.buildEnv coreModule
         --liftIO $ putStrLn $ pprShow $ ToLc.envMap env
         liftIO . putStrLn . showSDocUnsafe . ppr . fromJust $ term
         let toVarExpr = fmap ({-Dj.toDeBruijn .-} ToVarLc.toVarLc env) term
         putStep "toVarLc:" ppr toVarExpr
         --let reducedTyApps = fmap R.reduceTyApps toVarExpr

         let reRecursive = fmap Rr.reduceRecursive toVarExpr
         putStep "reduceRecursive:"  ppr reRecursive

         let reduced = fmap R.reduce reRecursive
         let noDataConTys = fmap DC.applyDataConTys reduced
         putStep "applyDataConTys:" ppr noDataConTys

         let command = fmap C.toCommandLc noDataConTys
         putStep "toCommand:" ppr command

         let reAgain = fmap C.reduceCommand command
         putStep "reAgain:" ppr reAgain
         let program = fmap Ps.lcToPseudo reAgain
         return program
         -}
