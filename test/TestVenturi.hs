{-# LANGUAGE QuasiQuotes #-}

module TestVenturi
  ( main
  )
where

import Language.Venturi.Generate

main = do
  generateCode "test/code/TestFunctions_vent.hs" "buildTree"
