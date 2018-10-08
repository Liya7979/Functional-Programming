{-modeq.hs
  author: Galiya Yeshmagambetova (s3360571)
  description: Gets an input of a series of integer pairs (a_i,m_i) and
  returns the smallest x such that any x mod m_i = a_i.
  The program uses Chinese remainder theorem to calculate x. The theorem
  states that if the divisors and remainders are known, it is possible to
  determine uniquely the remainder of the division of n by the product
  of these integers, under the condition that the divisors are pairwise coprime-}

solveModularEq :: [(Integer,Integer)] -> Integer
isLessProduct :: Integer->[(Integer, Integer)] -> Bool

-- computes whether x is less than product of the moduli
isLessProduct x tuplesList = if x > product(snd (unzip tuplesList)) then False
                                                                      else True
solveModularEq tuplesList = f tuplesList (fst (head tuplesList)) False
  where
    f tuplesList x True = x
    f tuplesList x isSolution =
      if isLessProduct x tuplesList == False then product(snd (unzip tuplesList))
        -- computes whether x satisfies x mod m_i = a_i,
        -- if there is the same number of (a,m) pairs such that x mod m_i = a_i
        -- as the number of (a,m) initially, the solution is found
         else if length [tuple | tuple <- tuplesList,
          mod x (snd tuple) == (fst tuple)] == length tuplesList
          then f tuplesList x True
          -- according to Chinese Remainder theorem, computes the next x to explore
          else f tuplesList (x+(snd (head tuplesList))) False

parseInput :: [String] -> [(Integer,Integer)]
parseInput [] = []
parseInput (a:b:xs) = (read a,read b):parseInput xs

main =  print . solveModularEq . parseInput . words =<< getLine
