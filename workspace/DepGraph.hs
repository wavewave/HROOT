module Main where

import HROOT.Data.Core.Class (core_classes)

import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import FFICXX.Generate.Dependency (mkModuleDepHighNonSource)
import FFICXX.Generate.Type.Class (Class (..), TemplateClass (..))
import Text.Dot

src, box, diamond :: String -> Dot NodeId
src     label = node $ [ ("shape","none"),("label",label) ]
box     label = node $ [ ("shape","box"),("style","rounded"),("label",label) ]
diamond label = node $ [("shape","diamond"),("label",label),("fontsize","10")]

main :: IO ()
main = do
  let format :: Either TemplateClass Class -> String
      format (Left t) = tclass_name t
      format (Right c) = class_name c
      mkDep :: Class -> (String, [String])
      mkDep c =
        let ds = mkModuleDepHighNonSource (Right c)
         in (format (Right c), fmap format ds)
      depmap = fmap mkDep core_classes
      allSyms =
        L.nub . L.sort $
          fmap fst depmap ++ concatMap snd depmap
      allISyms :: [(Int, String)]
      allISyms = zip [0..] allSyms
      symMap = HM.fromList allISyms
      symRevMap = HM.fromList $ fmap swap allISyms
      replace (c, ds) = do
        i <- HM.lookup c symRevMap
        js <- traverse (\d -> HM.lookup d symRevMap) ds
        pure (i, js)
      depmap' = mapMaybe replace depmap

  putStrLn $ showDot $ do
    attribute ("size","40,15")
    attribute ("rankdir","LR")
    cs <- traverse box allSyms
    for_ depmap' $ \(i, js) ->
      for_ js $ \j ->
        (cs !! i) .->. (cs !! j)
