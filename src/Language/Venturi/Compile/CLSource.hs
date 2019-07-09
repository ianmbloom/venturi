module Language.Venturi.Compile.CLSource
  ( clParamPtr
  , clParamVar
  , clFunc
  , clKernel
  , clFnCall
  , clVar
  , clVectorType
  , clMember
  , clCast
  , clReturn
  , clStm
  , clTuple2
  , clTuple4
  , clAdd
  , clMul
  , clSub
  , clEq
  , clLt
  , clGt
  , clInt
  , clGlobalId
  )
where

import Language.C.Quote.OpenCL
import qualified Language.C.Syntax as C
import Data.Loc
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

clParamPtr :: (SrcLoc -> C.TypeQual) -> (SrcLoc -> C.TypeSpec) -> String -> C.Param
clParamPtr memType ty name =
    C.Param (Just (C.Id name noLoc))
            (C.DeclSpec [] [memType noLoc] (ty noLoc) noLoc)
            (C.Ptr [] (C.DeclRoot noLoc) noLoc)
            noLoc

clParamVar :: (SrcLoc -> C.TypeSpec) -> String -> C.Param
clParamVar ty name =
    C.Param (Just (C.Id name noLoc))
            (C.DeclSpec [] [] (ty noLoc) noLoc)
            (C.DeclRoot noLoc)
            noLoc

clFunc :: (SrcLoc -> C.TypeSpec) -> String -> [C.Param] -> [C.BlockItem] -> C.Func
clFunc ty name params statements =
    C.Func (C.DeclSpec [] [] (ty noLoc) noLoc)
           (C.Id name noLoc)
           (C.DeclRoot noLoc)
           (C.Params params False noLoc)
           statements noLoc


clKernel :: (SrcLoc -> C.TypeSpec) -> String -> [C.Param] -> [C.BlockItem] -> C.Func
clKernel ty name params statements =
    C.Func (C.DeclSpec [] [C.TCLkernel noLoc] (ty noLoc) noLoc)
           (C.Id name noLoc)
           (C.DeclRoot noLoc)
           (C.Params params False noLoc)
           statements noLoc

clFnCall :: String -> [C.Exp] -> C.Exp
clFnCall name args = C.FnCall (C.Var (C.Id name noLoc) noLoc) args noLoc

clVar :: String -> C.Exp
clVar name = C.Var (C.Id name noLoc) noLoc

clVectorType :: (SrcLoc -> C.TypeSpec) -> Int -> SrcLoc -> C.TypeSpec
clVectorType ty size = C.Tnamed (C.Id ((pretty 0 . ppr $ ty noLoc) ++ show size) noLoc) []

clMember :: String -> C.Exp -> C.Exp
clMember name term = C.Member term (C.Id name noLoc) noLoc

clCast :: (SrcLoc -> C.TypeSpec) -> C.Exp -> C.Exp
clCast ty term = C.Cast (C.Type (C.DeclSpec [] [] (ty noLoc) noLoc)
                                (C.DeclRoot noLoc) noLoc) term noLoc


clAssign :: String -> C.Exp -> C.Exp
clAssign var term = C.Assign (clVar var) C.JustAssign term noLoc

clReturn :: C.Exp -> C.Stm
clReturn term = C.Return (Just term) noLoc

clStm :: C.Stm -> C.BlockItem
clStm term = C.BlockStm term

clTuple2 :: C.Exp -> C.Exp -> C.Exp
clTuple2 x y     = C.Seq x y noLoc

clTuple4 :: C.Exp -> C.Exp -> C.Exp -> C.Exp -> C.Exp
clTuple4 x y z w = C.Seq (C.Seq (C.Seq x y noLoc) z noLoc) w noLoc

clAdd :: C.Exp -> C.Exp -> C.Exp
clAdd a b = C.BinOp C.Add a b noLoc
clMul :: C.Exp -> C.Exp -> C.Exp
clMul a b = C.BinOp C.Mul a b noLoc
clSub :: C.Exp -> C.Exp -> C.Exp
clSub a b = C.BinOp C.Sub a b noLoc
clEq :: C.Exp -> C.Exp -> C.Exp
clEq a b = C.BinOp C.Eq a b noLoc
clLt :: C.Exp -> C.Exp -> C.Exp
clLt a b = C.BinOp C.Lt a b noLoc
clGt :: C.Exp -> C.Exp -> C.Exp
clGt a b = C.BinOp C.Gt a b noLoc

clInt :: Int -> C.Exp
clInt x = C.Const (C.IntConst "" C.Signed (fromIntegral x) noLoc) noLoc

clGlobalId :: Int -> C.Exp
clGlobalId i = clFnCall "get_global_id" [C.Const (C.IntConst "" C.Signed (fromIntegral i) noLoc) noLoc]
