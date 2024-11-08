data Ziffer = Null | Eins

instance Show Ziffer where
    show Null = "0"
    show Eins  = "1"

instance Eq Ziffer where
    (==) Null Null = True
    (==) Eins Eins = True
    (==) _    _    = False

instance Ord Ziffer where
    (<=) Eins Null = False
    (<=) _    _    = True

instance Enum Ziffer where
    toEnum 1 = Eins
    toEnum _ = Null
    fromEnum Null = 0
    fromEnum Eins = 1

instance Read Ziffer where
    readsPrec _ input =
        let input' = dropWhile (== ' ') input
        in case input' of
            ('0':rest) -> [(Null, rest)]
            ('1':rest) -> [(Eins , rest)]
            _          -> []



type Nat0        = Int
type CarryZiffer = Ziffer
type Borrow      = Bool
type Shift       = Nat0
data NatZahl     = L Ziffer
                   | E Ziffer NatZahl
newtype NatZahl' = NI NatZahl
type AccNatZahl  = NatZahl

instance Show NatZahl where
    show (L d)   = show d
    show (E d n) = show d ++ show n
instance Show NatZahl' where
    show (NI n) = show $ fromEnum n

instance Eq NatZahl where
    (==) (L d1)    (L d2)    = d1 == d2
    (==) (E d1 n1) (E d2 n2) = d1 == d2 && n1 == n2
    (==) n1        n2        =
        let (n1', n2') = pad n1 n2
        in n1' == n2'
instance Eq NatZahl' where
    (==) (NI n1) (NI n2) = n1 == n2

instance Ord NatZahl where
    (<=) (L d1) (L d2) = d1 <= d2
    (<=) n1 n2 =
        let ((E d1 n1'), (E d2 n2')) = pad n1 n2
        in if d2 > d1
           then True
           else d1 <= d2 && n1' <= n2'

instance Enum NatZahl where
    toEnum = binary
    fromEnum (L d) = fromEnum d
    fromEnum num@(E d n) =
        let numSize = size num
            factor  = 2^(numSize - 1)
        in (fromEnum d) * factor + (fromEnum n)

instance Num NatZahl where
    (+) n1 n2 = revert $ add (revert n1) (revert n2)
    (-) n1 n2 = trunc $ sub n1 n2
    (*) n1 n2 = trunc $ multiply n1 n2

    abs = id
    signum (E d n) =
        if d == Eins
        then (L Eins)
        else signum n
    signum l = l
    fromInteger = binary

instance Bounded NatZahl where
    minBound = L Null
    maxBound = error "There's no upper bound for NatZahls"  -- Theoretically there should be no upper bound; infinite

instance Read NatZahl where
    readsPrec _ input =
        let input' = dropWhile (== ' ') input
        in case reads input' of
            [(d :: Ziffer, ""  )] -> [(L d, "")]
            [(d :: Ziffer, rest)] ->
                case reads rest of
                    [(n :: NatZahl, rest')] -> [(E d n, rest')]
                    _ -> []
            _ -> []


{- Apply transform function n times on a Natnum. -}
apply :: (NatZahl -> NatZahl) -> Int -> NatZahl -> NatZahl
apply _ 0 n = n
apply f i n = apply f (i - 1) (f n)

{- Multiply two Natnums together. -}
multiply :: NatZahl -> NatZahl -> NatZahl
multiply n1 n2 =
    let (_, result) = multiply' n1 n2
    in result

multiply' :: NatZahl -> NatZahl -> (Shift, AccNatZahl)
multiply' n1 (L d) =
    if d == Eins
    then (1, n1)
    else (1, L Null)
multiply' n1 (E d n2) =
    let (shift, acc) = multiply' n1 n2
        appendNull   = append Null
        applyAppendNullShiftTimes = apply appendNull shift
        summand      = applyAppendNullShiftTimes n1
    in if d == Eins
       then (shift + 1, acc + summand)
       else (shift + 1, acc)

{- Borrow table for subtractWithBorrow.
 - Subtract the second Ziffer from the first based on the borrow of the last operation. -}
borrowTable :: Borrow -> Ziffer -> Ziffer -> (Borrow, Ziffer)
borrowTable False Null Null = (False, Null)
borrowTable False Null Eins = (True , Eins)
borrowTable False Eins Null = (False, Eins)
borrowTable False Eins Eins = (False, Null)
borrowTable True  Null Null = (True , Eins)  -- propagate the borrow; 0 becomes 10 (two), then 1, so it's actually 1 - 0 for the result
borrowTable True  Null Eins = (True , Null)  -- propagate the borrow; it's actually 1 - 1 for the result
borrowTable True  Eins Null = (False, Null)  -- 1 got borrowed, so it becomes 0; it's actually 0 - 0 for the result
borrowTable True  Eins Eins = (True , Eins)  -- 1 got borrowed; it becomes 0 - 1, so a borrow is needed

{- Subtract the second Natnum from the first using the borrow system.
 - Both Natnums must have the same size. -}
subtractWithBorrow :: NatZahl -> NatZahl -> (Borrow, NatZahl)
subtractWithBorrow (L d1) (L d2) =
    let (borrow, d') = borrowTable False d1 d2
    in (borrow, L d')
subtractWithBorrow (E d1 rest1) (E d2 rest2) =
    let (restBorrow, n') = subtractWithBorrow rest1 rest2
        (thisBorrow, d') = borrowTable restBorrow d1 d2
    in (thisBorrow, E d' n')

{- Subtract the second Natnum from the first.
 - Result might contain leading Nulls. -}
sub :: NatZahl -> NatZahl -> NatZahl
sub n1 n2 =
    let (n1', n2')       = pad n1 n2
        (borrow, result) = subtractWithBorrow n1' n2'
    in if borrow == True
       then L Null  -- We don't have negative numbers in Natnum
       else result

{- Truncate leading Nulls from a Natnum.
 - 0 is truncated to 0; 00 to 0; 000 to 0; ... -}
trunc :: NatZahl -> NatZahl
trunc (L d) = L d
trunc num@(E d n) =
    if d == Null
    then trunc n
    else num

{- Append a Ziffer at the end of a Natnum. -}
append :: Ziffer -> NatZahl -> NatZahl
append d1 (L d2)   = E d2 (L d1)
append d1 (E d2 n) = E d2 (append d1 n)

{- Revert a Natnum. -}
revert :: NatZahl -> NatZahl
revert n@(L _) = n
revert (E d n) = append d $ revert n

{- Carry table for addWithCarry. -}
carryTable :: CarryZiffer -> Ziffer -> Ziffer -> (CarryZiffer, Ziffer)
carryTable Null Null Null = (Null, Null)
carryTable Null Null Eins = (Null, Eins)
carryTable Null Eins Null = (Null, Eins)
carryTable Null Eins Eins = (Eins, Null)
carryTable Eins Null Null = (Null, Eins)
carryTable Eins Null Eins = (Eins, Null)
carryTable Eins Eins Null = (Eins, Null)
carryTable Eins Eins Eins = (Eins, Eins)

{- Add two Natnums together with carry starting from the least significant bit. -}
addWithCarry :: CarryZiffer -> NatZahl -> NatZahl -> NatZahl
addWithCarry c (L d1) (L d2) =
    let (c', d') = carryTable c d1 d2
    in if c' == Null
       then L d'
       else E d' (L c')
addWithCarry c (E d1 rest) (L d2) =
    let (c', d') = carryTable c d1 d2
    in E d' $ addWithCarry c' rest (L Null)
addWithCarry c n1@(L _) n2@(E _ _) =
    addWithCarry c n2 n1
addWithCarry c (E d1 rest1) (E d2 rest2) =
    let (c', d') = carryTable c d1 d2
    in E d' $ addWithCarry c' rest1 rest2

{- Add two Natnums together starting from the least significant bit. -}
add :: NatZahl -> NatZahl -> NatZahl
add = addWithCarry Null

{- Build a Natnum from a list of digits. -}
fromZiffers :: [Ziffer] -> NatZahl
fromZiffers []     = L Null
fromZiffers (d:[]) = L d
fromZiffers (d:ds) = E d (fromZiffers ds)

{- Transform an integer into a Natnum. -}
binary :: Integral a => a -> NatZahl
binary = binary' []

binary' :: Integral a => [Ziffer] -> a -> NatZahl
binary' ds 0 = fromZiffers ds
binary' ds i =
    let quotient = i `div` 2
        rest     = i `mod` 2
        d        = if rest == 1 then Eins else Null
    in binary' (d:ds) quotient

{- Get size of a Natnum word. -}
size :: NatZahl -> Int
size (L _)   = 1
size (E _ n) = 1 + (size n)

{- Pad a Natnum with a fixed number of zeros. -}
padWith :: Nat0 -> NatZahl -> NatZahl
padWith 0 n = n
padWith i n = padWith (i - 1) (E Null n)

{- Pad Natnums to have same size. -}
pad :: NatZahl -> NatZahl -> (NatZahl, NatZahl)
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

k  :: NatZahl  -> NatZahl'
k = NI
k' :: NatZahl' -> NatZahl
k' (NI n) = n

k1 :: Nat0   -> NatZahl
k1 = toEnum
k2 :: NatZahl -> Nat0
k2 = fromEnum

k3 :: Nat0    -> NatZahl'
k3 = NI . k1
k4 :: NatZahl' -> Nat0
k4 (NI n) = k2 n
