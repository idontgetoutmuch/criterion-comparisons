{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Frames
import           Frames.Melt (RDeleteAll, ElemOf)
import           Frames.InCore (RecVec)
import           Frames.CSV
import qualified Data.Foldable as F
import           Control.Lens (to, only,(^?),ix, toListOf, (^.), (.~), (%~), set)
import           Data.Vinyl (Rec(..))
import qualified Language.R as R
import           Language.R.QQ
import qualified Data.Text as T
import           Data.List
import           Data.Functor.Identity
import           Data.Vinyl.XRec (toHKD)
import           Data.Vinyl.TypeLevel as V
import           Data.Vinyl as V
import           H.Prelude as H
import           Language.R.QQ
import           Control.Monad.Primitive
import           Formatting
import           Data.Text.Lazy (toStrict)

import           Debug.Trace

tableTypes "Benchmarks" "/Users/dom/random-fu/v1.2-proposal.csv"

loadBenchmarks :: IO (Frame Benchmarks)
loadBenchmarks = inCoreAoS (readTable "/Users/dom/random-fu/release-v1.1-base.csv")

loadNewBenchmarks :: IO (Frame Benchmarks)
loadNewBenchmarks = inCoreAoS (readTable "/Users/dom/random-fu/v1.2-proposal.csv")

declareColumn "OldMean" ''Double
declareColumn "NewMean" ''Double
declareColumn "TestName" ''Text

interleave :: [a] -> [a] -> [a]
interleave = curry $ unfoldr g
  where
    g ([], [])   = Nothing
    g ([], (y:ys)) = Just (y, (ys, []))
    g (x:xs, ys) = Just (x, (ys, xs))

getFP :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
         Record rs -> Bool
getFP x = "Float" `T.isInfixOf`  y ||
          "Double" `T.isInfixOf`  y
  where
    y = x ^. name

getSoS :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
         Record rs -> Bool
getSoS x = "stdUniform" `T.isInfixOf` y &&
           "Double" `T.isInfixOf` y
  where
    y = x ^. name

getInt :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
          Record rs -> Bool
getInt x = "Int8"  `T.isInfixOf`  y ||
           "Int16" `T.isInfixOf`  y
  where
    y = x ^. name

getNormal :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
          Record rs -> Bool
getNormal x = "Normal"  `T.isInfixOf`  y
  where
    y = x ^. name

getPure :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
           Record rs -> Bool
getPure x = head zs == "pure"
  where
    y = x ^. name
    zs = T.splitOn "/" y

getMWC :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
           Record rs -> Bool
getMWC x = if length zs > 1
           then (zs !! 1) == "MWC"
           else False
  where
    y = x ^. name
    zs = T.splitOn "/" y

getGen :: RecElem Rec Name Name rs rs (RIndex Name rs) =>
          Text -> Record rs -> Bool
getGen s x = if length zs > 1
             then (zs !! 1) == s
             else False
  where
    y = x ^. name
    zs = T.splitOn "/" y

splitName :: Record '[Name] -> Record '[Name]
splitName = mapMono (\y -> let zs = T.splitOn "/" y in (mconcat (intersperse "/" (drop 2 zs))))

splitName1 :: Record '[Name] -> Record '[Name]
splitName1 = mapMono (\y -> let zs = T.splitOn "/" y in (mconcat (intersperse "/" (drop 1 zs))))

transform :: forall rs as bs . (as ⊆ rs, RDeleteAll as rs ⊆ rs)
             => (Record as -> Record bs) -> Record rs -> Record (RDeleteAll as rs V.++ bs)
transform f xs = rcast @(RDeleteAll as rs) xs `rappend` f (rcast xs)

retypeColumn :: forall x y rs . ( V.KnownField x
                                , V.KnownField y
                                , V.Snd x ~ V.Snd y
                                , ElemOf rs x
                                , RDelete x rs ⊆ rs)
  => Record rs -> Record (RDelete x rs V.++ '[y])
retypeColumn = transform @rs @'[x] @'[y] (\r -> (rgetField @x r &: V.RNil))

plotVals :: (RecVec rs,
             RecElem Rec Mean Mean rs rs (RIndex Mean rs),
             RecElem Rec Name Name rs rs (RIndex Name rs),
             RecElem Rec NewMean NewMean rs rs (RIndex NewMean rs)) =>
            FrameRec rs
         -> (Record rs -> Bool) -> ([String], [String], [Double])
plotVals df getCat = (es, ds, xs)
  where
    df1 = filterFrame getCat df
    as = F.toList $ fmap (rgetField @Mean) $ df1
    bs = F.toList $ fmap (rgetField @NewMean) $ df1
    xs = map (* 1000) $ interleave as bs
    cs = map T.unpack $ F.toList $ fmap (^. name) $ fmap (rsubset %~ splitName1) df1
    ds = concatMap (replicate 2) cs
    l = length ds
    es = take l (cycle ["Random 1.1", "Random 1.2"])

chart :: MonadR m => String -> [String] -> [String] -> [Double] ->
         m (SomeSEXP (PrimState m))
chart title ds6 es6 xs6 = do
  df <- [r| data <- data.frame(ds6_hs, es6_hs, xs6_hs) |]
  p1 <- [r| ggplot(df_hs, aes(fill=es6_hs, y=xs6_hs, x=ds6_hs, label=xs6_hs)) |]
  p2 <- [r| p1_hs + geom_bar(position="dodge", stat="identity") |]
  p3 <- [r| p2_hs + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) |]
  p4 <- [r| p3_hs + ylab("Milliseconds") + xlab("Type") +labs(fill = "Version") + scale_fill_manual(values=c("lightskyblue", "pink")) + geom_text(size = 2, position = position_dodge(width = 1), aes(y=xs6_hs + 2.0, label=signif(xs6_hs, digits=4), hjust=0), angle=90) |]
  p5 <- [r| p4_hs + ggtitle(title_hs) + theme(plot.title = element_text(hjust = 0.5)) |]
  return p5

main :: IO ()
main = do
  bs <- loadBenchmarks
  cs <- loadNewBenchmarks

  let df  = fmap (rsubset %~ splitName) $ filterFrame (getGen "StdGen") bs
      df1 = fmap (rsubset %~ splitName) $ filterFrame (getGen "StdGen") cs
      df2 = fmap (retypeColumn @'("Mean", Double) @'("NewMean", Double)) df1
      df3 = innerJoin @'[Name] df df2
      df4 = fmap (rcast @'[Name, Mean, NewMean]) df3
  writeCSV "perfComp.csv" df4

  let (es6, ds6, xs6) = plotVals df3 getNormal
      (esI, dsI, xsI) = plotVals df3 getSoS

  R.runRegion $ do
    [r| library(ggplot2) |]
    [r| library(gtable) |]
    [r| library(grid) |]

    p5 <- chart "Normal" ds6 es6 xs6
    p5I <- chart "Uniform" dsI esI xsI

    g5  <- [r| ggplotGrob(p5_hs) |]
    g5I <- [r| ggplotGrob(p5I_hs) |]
    g  <- [r| cbind(g5I_hs, g5_hs, size = "first") |]
    title <- [r| textGrob("random-fu Performance",gp=gpar(fontsize=30)) |]
    padding <- [r| unit(5,"mm") |]
    table <- [r| gtable_add_rows(g_hs, heights = grobHeight(title_hs) + padding_hs, pos = 0) |]
    h <-  [r| gtable_add_grob(table_hs, title_hs, 1, 1, 1, ncol(table_hs)) |]
    [r| grid.newpage() |]
    [r| grid.draw(h_hs) |]
    [r| ggsave(plot=g_hs, filename="diagrams/Compare1.1Vs1.2.svg") |]
    return ()

