module Cities
where

import Data.List

affinities :: [(User, [City])] -> [Result]
affinities [] = []
affinities list = [
    (pair, score) |
      pair <- sort uniquePairs,
      let score = frequencyOf (pair, allPairs)
  ]
    where
      allPairs = allCityPairs list
      uniquePairs = uniqueCityPairs list

type Result = (CityPair, AffinityScore)
type AffinityScore = Int
type City = String
type User = String
type CityPair = (City, City)

cityPairs :: (User, [City]) -> [CityPair]
cityPairs (user, cityList) = [(a,b) | (a:bs) <- tails cityList, b <- bs ]

allCityPairs :: [(User, [City])] -> [CityPair]
allCityPairs = concatMap cityPairs

uniqueCityPairs :: [(User, [City])] -> [CityPair]
uniqueCityPairs list = nub $ allCityPairs list

frequencyOf :: (CityPair, [CityPair]) -> Int
frequencyOf (pairToMatch, list) = length [ pair | pair <- list, pair == pairToMatch]