{-powdigits.hs
  author: Galiya Yeshmagambetova (s3360571)
  version: 1.0
  description: The program computes the last d digits of n^e -}
powDigits :: Integer -> Integer -> Int -> Integer
powDigits n e d = f 1 n e (10^d)
  where
    f temp n 1 d = mod (temp*n) d
    f temp n e d =
      if mod e 2 == 0 then f temp (mod (n^2) d) (div e 2) d
        else f (mod (temp*n) d) n (e-1) d

wrapper :: [String] -> Integer
wrapper (n:e:d:_) = powDigits (read n::Integer) (read e::Integer) (read d::Int)

main =  print . wrapper . words =<< getLine
