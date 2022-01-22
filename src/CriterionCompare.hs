{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Applicative (many)
import Data.Bifunctor      (first)
import Data.Foldable       (foldl')
import Data.Map            (Map)
import Data.Traversable    (for)
import System.FilePath     (dropExtension, takeFileName)
import System.IO           (stdout)

import qualified Data.Map.Strict                  as Map
import qualified Options.Applicative              as O
import qualified System.Console.ANSI              as ANSI
import qualified Text.PrettyPrint.Boxes.Annotated as B

import CsvParse
import Table
import Types

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { optRunNames :: [RunName]
    , optOutput   :: FilePath
    , optRunPaths :: [FilePath]
    }

options :: O.Parser Options
options = Options
    <$> many (O.strOption $ O.short 'l' <> O.long "label" <> O.help "label")
    <*> O.strOption (O.short 'o' <> O.long "output" <> O.metavar "FILE" <> O.help "output file name" <> O.value "-")
    <*> many (O.strArgument $ O.metavar "FILE" <> O.help "CSV file name")

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

flipFiniteMap :: (Ord a, Ord b) => Map a (Map b v) -> Map b (Map a v)
flipFiniteMap abv = Map.unionsWith Map.union
    [ Map.singleton b $ Map.singleton a v
    | (a, bv) <- Map.toList abv
    , (b, v)  <- Map.toList bv
    ]

-------------------------------------------------------------------------------
-- Geometric mean
-------------------------------------------------------------------------------

gmean :: Traversable f => f Stats -> Double
gmean = post . foldl' f (A 1.0 0 0) . fmap statsMean where
    f (A acc es n) d = A c (es + e) (n + 1) where
        (c, e) = split (acc * d)

    split :: Double -> (Double, Int)
    split d  = (d / 2 ^^ e, e) where e = exponent d

    post :: A -> Double
    post (A acc es n) = acc ** (1 / fromIntegral n)
                      * 2 ** (fromIntegral es / fromIntegral n)

-- | @A x e n@ is @n@ elements which product is @x * 2 ^^ e@
data A = A !Double !Int !Int

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- O.execParser $ O.info (O.helper <*> options) mempty
    supportsAnsi <- ANSI.hSupportsANSI stdout

    let annotated' :: [ANSI.SGR] -> String -> String
        annotated' [] str = str
        annotated' xs str = ANSI.setSGRCode xs ++ str ++ ANSI.setSGRCode []

    let annotated | supportsAnsi = annotated'
                  | otherwise    = const id

    let runs :: [(RunName, FilePath)]
        runs = zipWith f (map Just optRunNames ++ repeat Nothing) optRunPaths
          where
            f (Just n) fp = (n, fp)
            f Nothing  fp = (RunName $ dropExtension $ takeFileName fp, fp)

    let names' :: [RunName]
        names' = map fst runs

    case names' of
        [] -> return () -- nothing to do
        fname : names -> do
            results0 <- fmap Map.fromList $ for runs $ \(name, fp) ->
                (,) name . Map.fromList . map (first RowBenchName) <$> readResults fp

            let results1 :: Map RunName (Map RowName Stats)
                results1 = fmap addGMean results0 where
                    addGMean m = Map.insert RowMean (Stats (gmean m) 0 0 1e-7 0 0) m

            let results :: Map RowName (Map RunName Stats)
                results = flipFiniteMap results1

            let header :: Row V1 Box
                header = makeHeader fname names

            let table :: [Row V2 Box]
                table = makeTable fname names results

            let table1 :: Row V2 [Box]
                table1 = sequenceA table

            let table2 :: Row V2 Box
                table2 = case table1 of
                    Row f n xs -> Row
                        (B.vcat B.left f)
                        (B.vcat B.right n)
                        (fmap (fmap (B.vcat B.right)) xs)

            let table3 :: Row V1 Box
                table3 = hoistRow (\(V2 x y) -> V1 (x B.<+> y)) table2

            let table4 = pure (B.//) <*> header <*> table3

            putStr $ B.renderAnn annotated $ B.hsep 2 B.left table4
