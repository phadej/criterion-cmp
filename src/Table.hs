{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE RankNTypes     #-}
module Table where

import Control.Monad (ap)
import Data.Foldable (for_)
import Data.Map      (Map)
import Numeric       (showFFloat)

import qualified Data.Map.Strict                  as Map
import qualified Statistics.Distribution          as S
import qualified System.Console.ANSI              as ANSI
import qualified Text.PrettyPrint.Boxes.Annotated as B

import Types

type Box = B.Box [ANSI.SGR]

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

data V3 a = V3 a a a
  deriving (Show, Functor, Foldable)

instance Applicative V3 where
    pure x = V3 x x x
    V3 f1 f2 f3 <*> V3 x1 x2 x3 = V3 (f1 x1) (f2 x2) (f3 x3)

makeHeader
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Row V1 Box
makeHeader fname names =
    fmap B.text $ Row "Benchmark"  (getRunName fname) (map (V1 . getRunName) names)

makeTable
    :: RunName    -- ^ first run
    -> [RunName]  -- ^ other runs
    -> Map RowName (Map RunName Stats)
    -> [Row V2 Box]
makeTable fname names results = buildList $ do
    -- rows
    for_ (Map.toList results) $ \(rn, mp) ->
        for_ (Map.lookup fname mp) $ \fstats ->  do
            let fmean :: Double
                fmean = statsMean fstats

                f :: ND
                f = statsND fstats

                -- precision is now negative
                precision :: Int
                precision = min 0 $ round $ logBase 10 fmean

                -- rest benchmarks
                rest :: [V2 Box]
                rest = buildList $ do
                    for_ names $ \name -> case Map.lookup name mp of
                        Nothing    -> do
                            item (V2 (B.text "") (B.text ""))
                        Just stats -> do
                            let x = statsND stats
                            item $ V2 (showND precision x) (showP f x)

            -- name, first, rest
            item $ Row (B.text (getRowName rn)) (showND precision f) rest
  where
    showND :: Int -> ND -> Box
    showND p nd = B.text
        $ showFFloat (Just 3) (mul * (S.mean nd)) . showChar 'e' . shows p
        . showString " ±"
        . showFFloat (Just 2) (100 * S.stdDev nd / S.mean nd) . showChar '%'
        $ ""
      where mul = 10 ^ negate p

    showP :: ND -> ND -> Box
    showP orig curr
        | diff > 0   = mkBox $ '+' : showFFloat (Just 2) (100 *        diff) "%"
        | otherwise  = mkBox $ '-' : showFFloat (Just 2) (100 * negate diff) "%"
      where
        diff = (S.mean curr - S.mean orig) / S.mean orig
        mkBox | less            = B.ann (hl ANSI.Green) . B.text
              | more            = B.ann (hl ANSI.Red)   . B.text
              | otherwise       = B.text

        hl c = [ ANSI.SetConsoleIntensity ANSI.BoldIntensity
               , ANSI.SetColor ANSI.Foreground ANSI.Vivid c
               ]

        -- Pr(curr < 0.97 * orig) = Pr (curr - 0.97 * orig < 0)
        -- i.e. 3% change with 99% confidence
        less :: Bool
        less = S.cumulative (subND curr (scaleND 0.97 orig)) 0 > 0.99

        more :: Bool
        more = S.cumulative (subND orig (scaleND 0.97 curr)) 0 > 0.99

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
