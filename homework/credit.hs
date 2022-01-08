import Data.List

toDigit :: Integer -> [Integer]
toDigit a = f a []
  where
    f 0 x = x
    f a x =
      let
        rem = a `mod` 10
        a' = a `div` 10
      in
        f a' (rem : x)

toDigitRev = reverse . toDigit

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = (x : (y*2) : (doubleEveryOther' xs))

doubleEveryOther = reverse . doubleEveryOther' . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . (map sum) . map toDigit

validate :: Integer -> Bool
validate a = case (sumDigits . doubleEveryOther . toDigit $ a) `mod` 10 of
  0 -> True
  _ -> False
  
