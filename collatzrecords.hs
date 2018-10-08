{-collatzrecords.hs
  author: Galiya Yeshmagambetova (s3360571)
  description: The program computes the nth Collatz record-}
nthCollatzRecord :: Int -> Integer
nCollatzRecordSteps :: Int -> Integer
nCollatzRecordSteps number = f 0 number
  where
    f steps 1 = steps
    f steps number =
      if ((mod number 2) == 0) then f (steps+1) (div number 2)
        else f (steps+1) (3*number+1)

nthCollatzRecord n = f n 1 0
  where
    f 0 current_number maxSteps = toInteger(current_number - 1)
    --we return current_number-1 because of recursion
    f n current_number maxSteps =
      if (nCollatzRecordSteps current_number > maxSteps)
        then f (n-1) (current_number+1) (nCollatzRecordSteps current_number)
        else f n (current_number+1) maxSteps

wrapper :: [String] -> Integer
wrapper (n:_) = nthCollatzRecord (read n::Int)

main =  print . wrapper . words =<< getLine
