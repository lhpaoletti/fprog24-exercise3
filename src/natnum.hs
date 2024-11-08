{- Natnum module of exercise 3.
 - author: Luis Henrique Paoletti
 - date: 04.11.24
 -}
module Natnum where
import Digit

type Nat0       = Int
type CarryDigit = Digit
type Borrow     = Bool
type Shift      = Nat0
data Natnum     = L Digit           -- as in 'last digit'
                  | A Digit Natnum  -- as in 'a digit'
newtype Natnum' = NI Natnum         -- NI as in 'new identity'
type AccNatnum  = Natnum

instance Show Natnum where
    show (L d)   = show d
    show (A d n) = show d ++ show n
instance Show Natnum' where
    show (NI n) = show $ fromEnum n

instance Eq Natnum where
    (==) (L d1)    (L d2)    = d1 == d2
    (==) (A d1 n1) (A d2 n2) = d1 == d2 && n1 == n2
    (==) n1        n2        =
        let (n1', n2') = pad n1 n2
        in n1' == n2'
instance Eq Natnum' where
    (==) (NI n1) (NI n2) = n1 == n2

instance Ord Natnum where
    (<=) (L d1) (L d2) = d1 <= d2
    (<=) n1 n2 =
        let ((A d1 n1'), (A d2 n2')) = pad n1 n2
        in if d2 > d1
           then True
           else d1 <= d2 && n1' <= n2'

instance Enum Natnum where
    toEnum = binary
    fromEnum (L d) = fromEnum d
    fromEnum num@(A d n) =
        let numSize = size num
            factor  = 2^(numSize - 1)
        in (fromEnum d) * factor + (fromEnum n)

instance Num Natnum where
    (+) n1 n2 = revert $ add (revert n1) (revert n2)
    (-) n1 n2 = trunc $ sub n1 n2
    (*) n1 n2 = trunc $ multiply n1 n2

    abs = id
    signum (A d n) =
        if d == One
        then (L One)
        else signum n
    signum l = l
    fromInteger = binary

instance Bounded Natnum where
    minBound = L Zero
    maxBound = error "There's no upper bound for Natnums"  -- Theoretically there should be no upper bound; infinite

instance Read Natnum where
    readsPrec _ input =
        let input' = dropWhile (== ' ') input
        in case reads input' of
            [(d :: Digit, ""  )] -> [(L d, "")]
            [(d :: Digit, rest)] ->
                case reads rest of
                    [(n :: Natnum, rest')] -> [(A d n, rest')]
                    _ -> []
            _ -> []


{- Apply transform function n times on a Natnum. -}
apply :: (Natnum -> Natnum) -> Int -> Natnum -> Natnum
apply _ 0 n = n
apply f i n = apply f (i - 1) (f n)

{- Multiply two Natnums together. -}
multiply  :: Natnum -> Natnum -> Natnum
multiply n1 n2 =
    let (_, result) = multiply' n1 n2
    in result

multiply' :: Natnum -> Natnum -> (Shift, AccNatnum)
multiply' n1 (L d) =
    if d == One
    then (1, n1)
    else (1, L Zero)
multiply' n1 (A d n2) =
    let (shift, acc) = multiply' n1 n2
        appendZero   = append Zero
        applyAppendZeroShiftTimes = apply appendZero shift
        summand      = applyAppendZeroShiftTimes n1
    in if d == One
       then (shift + 1, acc + summand)
       else (shift + 1, acc)

{- Borrow table for subtractWithBorrow.
 - Subtract the second Digit from the first based on the borrow of the last operation. -}
borrowTable :: Borrow -> Digit -> Digit -> (Borrow, Digit)
borrowTable False Zero Zero = (False, Zero)
borrowTable False Zero One  = (True , One )
borrowTable False One  Zero = (False, One )
borrowTable False One  One  = (False, Zero)
borrowTable True  Zero Zero = (True , One )  -- propagate the borrow; 0 becomes 10 (two), then 1, so it's actually 1 - 0 for the result
borrowTable True  Zero One  = (True , Zero)  -- propagate the borrow; it's actually 1 - 1 for the result
borrowTable True  One  Zero = (False, Zero)  -- 1 got borrowed, so it becomes 0; it's actually 0 - 0 for the result
borrowTable True  One  One  = (True , One )  -- 1 got borrowed; it becomes 0 - 1, so a borrow is needed

{- Subtract the second Natnum from the first using the borrow system.
 - Both Natnums must have the same size. -}
subtractWithBorrow :: Natnum -> Natnum -> (Borrow, Natnum)
subtractWithBorrow (L d1) (L d2) =
    let (borrow, d') = borrowTable False d1 d2
    in (borrow, L d')
subtractWithBorrow (A d1 rest1) (A d2 rest2) =
    let (restBorrow, n') = subtractWithBorrow rest1 rest2
        (thisBorrow, d') = borrowTable restBorrow d1 d2
    in (thisBorrow, A d' n')

{- Subtract the second Natnum from the first.
 - Result might contain leading Zeros. -}
sub :: Natnum -> Natnum -> Natnum
sub n1 n2 =
    let (n1', n2')       = pad n1 n2
        (borrow, result) = subtractWithBorrow n1' n2'
    in if borrow == True
       then L Zero  -- We don't have negative numbers in Natnum
       else result

{- Truncate leading Zeros from a Natnum.
 - 0 is truncated to 0; 00 to 0; 000 to 0; ... -}
trunc :: Natnum -> Natnum
trunc (L d) = L d
trunc num@(A d n) =
    if d == Zero
    then trunc n
    else num

{- Append a Digit at the end of a Natnum. -}
append :: Digit -> Natnum -> Natnum
append d1 (L d2)   = A d2 (L d1)
append d1 (A d2 n) = A d2 (append d1 n)

{- Revert a Natnum. -}
revert :: Natnum -> Natnum
revert n@(L _) = n
revert (A d n) = append d $ revert n

{- Carry table for addWithCarry. -}
carryTable :: CarryDigit -> Digit -> Digit -> (CarryDigit, Digit)
carryTable Zero Zero Zero = (Zero, Zero)
carryTable Zero Zero One  = (Zero, One )
carryTable Zero One  Zero = (Zero, One )
carryTable Zero One  One  = (One , Zero)
carryTable One  Zero Zero = (Zero, One )
carryTable One  Zero One  = (One , Zero)
carryTable One  One  Zero = (One , Zero)
carryTable One  One  One  = (One , One )

{- Add two Natnums together with carry starting from the least significant bit. -}
addWithCarry :: CarryDigit -> Natnum -> Natnum -> Natnum
addWithCarry c (L d1) (L d2) =
    let (c', d') = carryTable c d1 d2
    in if c' == Zero
       then L d'
       else A d' (L c')
addWithCarry c (A d1 rest) (L d2) =
    let (c', d') = carryTable c d1 d2
    in A d' $ addWithCarry c' rest (L Zero)
addWithCarry c n1@(L _) n2@(A _ _) =
    addWithCarry c n2 n1
addWithCarry c (A d1 rest1) (A d2 rest2) =
    let (c', d') = carryTable c d1 d2
    in A d' $ addWithCarry c' rest1 rest2

{- Add two Natnums together starting from the least significant bit. -}
add :: Natnum -> Natnum -> Natnum
add = addWithCarry Zero

{- Build a Natnum from a list of digits. -}
fromDigits :: [Digit] -> Natnum
fromDigits []     = L Zero
fromDigits (d:[]) = L d
fromDigits (d:ds) = A d (fromDigits ds)

{- Transform an integer into a Natnum. -}
binary  :: Integral a => a -> Natnum
binary = binary' []

binary' :: Integral a => [Digit] -> a -> Natnum
binary' ds 0 = fromDigits ds
binary' ds i =
    let quotient = i `div` 2
        rest     = i `mod` 2
        d        = if rest == 1 then One else Zero
    in binary' (d:ds) quotient

{- Get size of a Natnum word. -}
size :: Natnum -> Int
size (L _)   = 1
size (A _ n) = 1 + (size n)

{- Pad a Natnum with a fixed number of zeros. -}
padWith :: Nat0 -> Natnum -> Natnum
padWith 0 n = n
padWith i n = padWith (i - 1) (A Zero n)

{- Pad Natnums to have same size. -}
pad :: Natnum -> Natnum -> (Natnum, Natnum)
pad n1@(L _) n2@(L _) = (n1, n2)
pad n1 n2
  | size1 < size2 = let padded1 = padWith (size2 - size1) n1
                    in (padded1, n2)
  | size2 < size1 = let padded2 = padWith (size1 - size2) n2
                    in (n1, padded2)
  | otherwise     = (n1, n2)
    where size1 = size n1
          size2 = size n2


-- A3 & A4 --

c  :: Natnum  -> Natnum'
c = NI
c' :: Natnum' -> Natnum
c' (NI n) = n

c1 :: Nat0   -> Natnum
c1 = toEnum
c2 :: Natnum -> Nat0
c2 = fromEnum

c3 :: Nat0    -> Natnum'
c3 = NI . c1
c4 :: Natnum' -> Nat0
c4 (NI n) = c2 n
