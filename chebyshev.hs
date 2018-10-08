{-chebyshev.hs
  author: Galiya Yeshmagambetova (s3360571)
  version: 1.0
  description: This program returns Chebyshev number calculated by the following
               formula: Cn = 4*Cn-1 - Cn-2 -}
chebyshev :: Integer -> Integer

chebyshev n = f n 1 2
  where
    f 0 a b = a
    f n a b = f (n - 1) (4*a-b) (a)

wrapper :: [String] -> Integer
wrapper (a:_) = chebyshev (read a::Integer)

main =  print . wrapper . words =<< getLine
