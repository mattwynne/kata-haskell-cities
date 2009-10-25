module Cities
where

import Data.List

affinities :: [(User, [City])] -> [Result]
affinities [] = []
affinities inputRows = [
    (pair, score) |
      pair <- sort uniquePairs,
      let score = frequencyOf (pair, allPairs)
  ]
    where
      allPairs = allCityPairs (cityLists inputRows)
      uniquePairs = nub allPairs

      cityLists :: [(User, [City])] -> [[City]]
      cityLists = map snd

type Result = (CityPair, AffinityScore)
type AffinityScore = Int
type City = String
type User = String
type CityPair = (City, City)

cityPairs :: [City] -> [CityPair]
cityPairs cityList = [(a,b) | (a:bs) <- tails cityList, b <- bs ]

allCityPairs :: [[City]] -> [CityPair]
allCityPairs = concatMap cityPairs

frequencyOf :: (CityPair, [CityPair]) -> Int
frequencyOf (pairToMatch, list) = length [ pair | pair <- list, pair == pairToMatch ]