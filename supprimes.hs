{-supprimes.hs
  author: Galiya Yeshmagambetova (s3360571)
  description: The program computes nth super prime where super prime is a current_number
  that remains a prime even after dropping one digit at any position.-}
isPrime :: Int -> Bool
qntDigits :: Int -> Int
nthSuperPrime :: Int -> Integer
splitAtIdx :: Int -> Int -> Int
isPrimeAlways :: Int -> Bool

isPrime n = f n 2
  where
    f 1 i = False
    f 2 i = True
    f n i =
      if mod n i == 0 then False
        else if i * i > n then True
          else f n (i + 1)

-- computes the number of digits
qntDigits n = f n 0
  where
    f 0 qnt = qnt
    f n qnt = f (div n 10) (qnt + 1)

-- drops one digit at given index
splitAtIdx n idx = read ((fst (splitAt idx (show n))) ++ (tail (snd (splitAt idx (show n))))) :: Int

-- checks whether the number is prime after dropping one digit at any position
isPrimeAlways n = f n ((qntDigits n) - 1)
  where
    -- since index 0 exists, we go to -1
    f n (-1) = True
    f n idx = if isPrime (splitAtIdx n idx) then f n (idx - 1)
      else False

nthSuperPrime n = f n 23
  where
    f 0 currNum = toInteger currNum - 1
    f n currNum = if isPrime currNum && isPrimeAlways currNum then f (n - 1) (currNum + 1)
      else f n (currNum + 1)

wrapper :: [String] -> Integer


wrapper (n:_) = nthSuperPrime (read n::Int)

main =  print . wrapper . words =<< getLine
