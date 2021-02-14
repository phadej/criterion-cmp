{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
module Table where

import Control.Monad (ap)
import Data.Foldable (for_)
import Data.Map      (Map)
import Numeric       (showFFloat)

import qualified Text.PrettyPrint.Boxes as B
import qualified Data.Map.Strict as Map

import Types

-- | Row of things.
--
-- Name, first column, and then pair of columns
data Row f a = Row a a [f a]
  deriving (Show, Functor, Foldable)

hoistRow :: (f a -> g a) -> Row f a -> Row g a
hoistRow f (Row x y zs) = Row x y (map f zs)

instance Applicative f => Applicative (Row f) where
    pure x = Row x x (repeat (pure x))

    Row f1 f2 fs <*> Row x1 x2 xs =
        Row (f1 x1) (f2 x2) (zipWith (<*>) fs xs)

newtype V1 a = V1 a
  deriving (Show, Functor, Foldable)

instance Applicative V1 where
    pure x = V1 x
    V1 f1 <*> V1 x1 = V1 (f1 x1)

data V2 a = V2 a a
  deriving (Show, Functor, Foldable)

instance Applicative V2 where
    pure x = V2 x x
    V2 f1 f2 <*> V2 x1 x2 = V2 (f1 x1) (f2 x2)

makeHeader
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Row V1 B.Box
makeHeader fname names =
    fmap B.text $ Row "Benchmark"  (getRunName fname) (map (V1 . getRunName) names)

makeTable
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Map RowName (Map RunName Stats)
    -> [Row V2 B.Box]
makeTable fname names results = map (fmap B.text) $ buildList $ do
    -- rows
    for_ (Map.toList results) $ \(rn, mp) ->
        for_ (Map.lookup fname mp) $ \fstats ->  do
            let fmean :: Double
                fmean = statsMean fstats

                precision :: Int
                precision = round $ logBase 10 fmean

                -- rest benchmarks
                rest :: [V2 String]
                rest = buildList $ do
                    for_ names $ \name -> case Map.lookup name mp of
                        Nothing    -> do
                            item (V2 "" "")
                        Just stats -> do
                            let mean = statsMean stats
                            item $ V2 (showD precision mean) (showP fmean mean)

            -- name, first, rest
            item $ Row (getRowName rn) (showD precision fmean) rest
  where
    showD :: Int -> Double -> String
    showD p d = showFFloat (Just 3) (mul * d) . showChar 'e' . shows p $ ""
      where mul = 10 ^ negate p

    showP :: Double -> Double -> String
    showP orig curr
        | curr > orig  = '+' : showFFloat (Just 2) (100 * (curr - orig) / orig) "%"
        | otherwise    = '-' : showFFloat (Just 2) (100 * (orig - curr) / orig) "%"

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
