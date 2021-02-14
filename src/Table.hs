{-# LANGUAGE RankNTypes #-}
module Table where

import Control.Monad (ap)
import Data.Foldable (for_)
import Data.List     (intercalate)
import Data.Map      (Map)
import Numeric       (showFFloat)

import qualified Data.Map.Strict as Map

import Types

makeTable
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Map RowName (Map RunName Stats)
    -> [[String]]
makeTable fname names results = buildList $ do
    -- header
    item $ buildList $ do
        item "Benchmark"
        item (getRunName fname)
        for_ names $ \name -> do
            item (getRunName name)
            item ""

    -- rows
    for_ (Map.toList results) $ \(rn, mp) ->
        for_ (Map.lookup fname mp) $ \fstats -> item $ buildList $ do
            -- benchmark and first value
            item (getRowName rn)
            let fmean = statsMean fstats
            let precision :: Int
                precision = round $ logBase 10 fmean

            item $ showD precision fmean

            -- rest benchmarks
            for_ names $ \name -> case Map.lookup name mp of
                Nothing    -> do
                    item ""
                    item ""
                Just stats -> do
                    let mean = statsMean stats
                    item $ showD precision mean
                    item $ showP fmean mean
  where
    showD :: Int -> Double -> String
    showD p d = showFFloat (Just 3) (mul * d) . showChar 'e' . shows p $ ""
      where mul = 10 ^ negate p

    showP :: Double -> Double -> String
    showP orig curr
        | curr > orig  = '+' : showFFloat (Just 2) (100 * (curr - orig) / orig) "%"
        | otherwise    = '-' : showFFloat (Just 2) (100 * (orig - curr) / orig) "%"

-- https://oleg.fi/gists/posts/2019-04-28-tabular.html
--
-- unfortunately this doesn't allow colspans
tabular :: [[String]] -> String
tabular zs = unlines rs where
    (cs, ws, rs) = foldr go (0, repeat 0, []) zs

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go x (c, w, ys) =
        (max c (length x), zipWith max w (map length x ++ repeat 0),
         unwords' (take cs (zipWith' x ws)) : ys)

    fr s n = replicate (n - length s) ' ' ++ s
    fl s n = s ++ replicate (n - length s) ' '

    zipWith' :: [String] -> [Int] -> [String]
    zipWith' (s:ss) (n:ns) = fl s n : zipWith fr ss ns
    zipWith' _      _      = []

    -- two spaces instead one
    unwords' = intercalate "  "

-------------------------------------------------------------------------------
-- List Builder
-------------------------------------------------------------------------------

newtype ListBuilder x a = LB { unLB :: forall r. (([x] -> [x]) -> a -> r) -> r }

instance Functor (ListBuilder x) where
    fmap f (LB k) = LB $ k $ \endo a k' -> k' endo (f a)

instance Applicative (ListBuilder x) where
    pure x = LB $ \f -> f id x
    (<*>)  = ap

instance Monad (ListBuilder x) where
    return = pure

    m >>= k =
        LB $ \r ->
        unLB m $ \endo1 a ->
        unLB (k a) $ \endo2 b ->
        r (endo1 . endo2) b

buildList :: ListBuilder x () -> [x]
buildList (LB f) = f $ \endo _ -> endo []

item :: x -> ListBuilder x ()
item x = LB $ \f -> f (x :) ()
