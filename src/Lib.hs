module Lib
    ( getPrimes
    , formatOutput
    ) where

import           Data.List       (intercalate, transpose)
import           Data.List.Split (chunksOf)

getPrimes :: Int -> Int -> Int -> [[Integer]]
getPrimes totalCount pageSize numColumns = formattedPages
  where
    rawPages       = chunksOf pageSize $ take totalCount primes
    formattedPages = concatMap (formatPage n) rawPages
    n              = pageSize `div` numColumns

-- getPrimesString :: Int -> Int -> Int -> String
-- getPrimesString totalCount pageSize numColumns = formatOutput $ getPrimes totalCount pageSize numColumns

-- infinite stream of primes
-- stolen from https://stackoverflow.com/a/3596536/2899222
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

-- formats a page into columns.
formatPage :: Int -> [Integer] -> [[Integer]]
formatPage chunkSize page = transpose $ chunksOf chunkSize page

formatOutput :: [[Integer]] -> Int -> Int -> Int -> String
formatOutput nums totalCount pageSize numColumns = concat $ zipWith (++) headers pages
  where
    lines        = fmap mkNumLine nums
    groupedLines = chunksOf (pageSize `div` numColumns) lines
    pages        = fmap mkPage groupedLines
    headers      = mkHeaderLines totalCount $ length groupedLines

mkPage :: [String] -> String
mkPage strs = concat strs

mkNumLine :: Show a => [a] -> String
mkNumLine as = intercalate ", " (fmap show as) ++ "\n"

mkHeaderLines :: Int -> Int -> [String]
mkHeaderLines total n = fmap (mkHeaderLine total) [1..n]
  where
    mkHeaderLine total pageNum = "the first " ++ show total ++ " prime numbers --- page " ++ show pageNum ++ "\n"
