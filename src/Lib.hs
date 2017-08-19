module Lib
    ( getPrimes
    , getPrimesString
    ) where

import           Data.List           (intercalate, transpose)
import           Data.List.Split     (chunksOf)
import           Data.Numbers.Primes (primes)

getPrimes :: Int -> Int -> Int -> [[[Integer]]]
getPrimes totalCount pageSize numColumns = formattedPages
  where
    rawPages       = chunksOf pageSize $ take totalCount primes
    formattedPages = fmap (mkColumns chunkSize) rawPages
    chunkSize      = pageSize `div` numColumns

getPrimesString :: Int -> Int -> Int -> String
getPrimesString totalCount pageSize numColumns = concat $ zipWith (++) headers pages
  where
    pages    = (fmap . concatMap) mkNumLine nums
    headers  = mkHeaderLines totalCount numPages
    numPages = totalCount `div` pageSize
    nums     = getPrimes totalCount pageSize numColumns

mkColumns :: Int -> [a] -> [[a]]
mkColumns chunkSize page = transpose $ chunksOf chunkSize page

mkNumLine :: Show a => [a] -> String
mkNumLine as = intercalate ", " (fmap show as) ++ "\n"

mkHeaderLines :: Int -> Int -> [String]
mkHeaderLines total n = fmap (mkHeaderLine total) [1..n]
  where
    mkHeaderLine total pageNum = "the first " ++ show total ++ " prime numbers --- page " ++ show pageNum ++ "\n"
