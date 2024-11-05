{- Natnum module of exercise 3.
 - author: Luis Henrique Paoletti
 - date: 04.11.24
 -}
module Natnum where
import Digit

type Nat0   = Int
data Natnum = L Digit           -- as in 'last digit'
              | A Digit Natnum  -- as in 'a digit'

instance Show Natnum where
  show (L d)   = show d
  show (A d n) = show d ++ show n

instance Eq Natnum where
  (==) (L d1)    (L d2)    = d1 == d2
  (==) (A d1 n1) (A d2 n2) = d1 == d2 && n1 == n2
  (==) _         _         = False

instance Ord Natnum where
  (<=) (L d1) (L d2) = d1 <= d2
  (<=) n1 n2 = let ((A d1 n1'), (A d2 n2')) = pad n1 n2
               in if d2 > d1
                  then True
                  else d1 <= d2 && n1' <= n2'

instance Enum Natnum where
  toEnum = binary
  fromEnum (L d)       = fromEnum d
  fromEnum num@(A d n) = let numSize = size num
                             factor  = 2^(numSize - 1)
                         in (fromEnum d) * factor + (fromEnum n)


{- Build a Natnum from a list of digits. -}
fromDigits :: [Digit] -> Natnum
{- Transform an integer into a Natnum. -}
binary  :: Int -> Natnum
binary' :: [Digit] -> Int -> Natnum
{- Get size of a Natnum word. -}
size :: Natnum -> Int
{- Pad a Natnum with a fixed number of zeros. -}
padWith :: Nat0 -> Natnum -> Natnum
{- Pad Natnums to have same size. -}
pad :: Natnum -> Natnum -> (Natnum, Natnum)


fromDigits []     = L Zero
fromDigits [Zero] = L Zero
fromDigits [One]  = L One
fromDigits (d:ds) = A d (fromDigits ds)

binary = binary' []
binary' ds 0 = fromDigits ds
binary' ds i = let quotient = i `div` 2
                   rest     = i `mod` 2
                   d        = if rest == 1 then One else Zero
               in binary' (d:ds) quotient

size (L _)   = 1
size (A _ n) = 1 + (size n)

padWith 0 n = n
padWith i n = padWith (i - 1) (A Zero n)

pad n1@(L _) n2@(L _) = (n1, n2)
pad n1 n2
  | size1 < size2 = let padded1 = padWith (size2 - size1) n1
                    in (padded1, n2)
  | size2 < size1 = let padded2 = padWith (size1 - size2) n2
                    in (n1, padded2)
  | otherwise     = (n1, n2)
    where size1 = size n1
          size2 = size n2
