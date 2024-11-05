{- Digit module of exercise 3.
 - author: Luis Henrique Paoletti
 - date: 04.11.24
 -}
module Digit where

data Digit = Zero | One

instance Show Digit where
  show Zero = "0"
  show One  = "1"

instance Eq Digit where
  (==) Zero Zero = True
  (==) One  One  = True
  (==) _    _    = False

instance Ord Digit where
  (<=) One  Zero = False
  (<=) _    _    = True

instance Enum Digit where
  toEnum 1 = One
  toEnum _ = Zero
  fromEnum Zero = 0
  fromEnum One  = 1
