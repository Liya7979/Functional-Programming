nthPalPrime :: Int -> Integer
isPrime :: Int -> Bool
isPalindrome :: Int -> Bool
qntDigits :: Int -> Int

isPrime n = f n 2
  where
    f 1 i = False
    f 2 i = True
    f n i =
      if mod n i == 0 then False
        else if i*i > n then True
          else f n (i+1)

isPalindrome n = if reverse (show n) == show n then True else False

qntDigits n = f n 0
  where
    f 0 qnt = qnt
    f n qnt = f (div n 10) (qnt + 1)

nthPalPrime n = f n 1
  where
    f 0 current_num = toInteger current_num-1
    f n  current_num = f (if isPalindrome current_num && isPrime current_num
      then (n-1) else n)
        (if current_num > 999 && mod (qntDigits current_num) 2 == 0
          then (current_num*10) else (current_num + 1))

wrapper :: [String] -> Integer
wrapper (a:_) = nthPalPrime (read a::Int)

main =  print . wrapper . words =<< getLine
