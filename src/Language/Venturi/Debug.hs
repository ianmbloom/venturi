module Language.Venturi.Debug
  ( tr
  , trWith
  , trPpr
  , trStack
  , tcWith
  , tc
  , tcP
  , trP
  , trPW
  , trace
  , showSDocUnsafe
  )
where

import Outputable
import Debug.Trace

trWith :: (a -> String) -> String -> a -> a
trWith f m x = trace (m++":\n"++ f x ++ "\n") x

tr :: Show a => String -> a -> a
tr = trWith show

trPpr :: (Outputable a) => String -> a -> a
trPpr = trWith (showSDocUnsafe . ppr)

trStack :: (Outputable a) => String -> [a] -> [a]
trStack = trWith (showSDocUnsafe . vcat . map ppr)

tcWith :: (a -> String) -> String -> a -> a
tcWith f m x = trace (m++"-->") $
               trace ("-->"++m++"   "++(f x)) x

tc :: Show a => String -> a -> a
tc = tcWith show

tcP :: (Outputable a) => String -> a -> a
tcP = tcWith (showSDocUnsafe . ppr)

trP :: (Outputable a) => String -> a -> a
trP = trWith (showSDocUnsafe . ppr)

trPW f = trWith (showSDocUnsafe . f)
