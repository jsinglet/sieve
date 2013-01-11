--------------------------------------------------------------------
-- |
-- Module    : Data.Sieve
-- Copyright : (c) John L. Singleton 2013
-- License   : GPL-2
--
-- Maintainer: John L. Singleton <jsinglet@gmail.com>
-- Stability : provisional
-- Portability: portable 
--
-- An implementation of the Sieve abstract data type in Haskell. 
--
-- A Sieve is a data type with properties analogous to a physical Sieve and it is useful
-- for building up lists of data wherein a specific constraint must be met that cannot be achieved 
-- using normal type semantics. A Sieve encapsulates a list that can only hold a certain type, specified by a
-- identity function and is preferable to creating or building up lists by using conditional blocks or by the
-- progressive use of 'filter'. This is especially advantageous if a list is to be passed around and used as an
-- accumulator. In such a configuration, the original declaring type is passed around with the Sieve so that it
-- can be used transparently in subsequent areas of the program.
--
-- A Sieve is especially useful for applications that:
--
-- * Will hold onto a list for longer than a single function
--
-- * Need to perform asynchronous stream processing
-- 
-- Consider the following example wherein we wish to create and maintain a list with 'Integer' values greater than 2. 
--
-- > f2 :: Sieve Int -> Sieve Int
-- > f2 s = [7,8,1] ++? s
-- 
-- > f1 :: Sieve Int 
-- > f1 = let numbersGreaterThanTwo = newSieve (\x -> x > 2) [1,2,3] in f2 $ [4,5,6] ++? numbersGreaterThanTwo
--
-- This example produces the list: @[7,8,4,5,6,3]@.
--------------------------------------------------------------------

module Data.Sieve (

  -- * Sieve Backing Types
  Sieve,

  -- * Constructing a New Sieve
  newSieve,

  -- * Accessing Pieces of the Sieve
  toList,

  relation,
  
  -- * Building Up Lists With a Sieve
  (++?)

  
  ) where

-- | The type that backs created Sieves. 
data Sieve a = Sieve {
   relation :: (a -> Bool) -- ^ Returns the backing relation used to determine if a given element is a member of the list.
  ,toList      :: [a] -- ^ The 'toList' function returns the resulting backing list.
  }

-- | The function 'newSieve' is the preferred method of creating a new 'Sieve'. The sieve that is constructed will immediately filter the list passed as the second parameter to 'newSieve'.
newSieve ::
  (a -> Bool) -- ^ A unary function that returns 'True' if this element should be allowed into the 'Sieve', and 'False' if not.
  -> [a] -- ^ The initial list to populate this 'Sieve' with. Note that the list will be immediately filtered.
  -> Sieve a -- ^ The resulting 'Sieve'.
newSieve a xs = xs ++? Sieve a []

-- | The operator for building up lists with a Sieve. The operator should be read as \"Conditionally Add.\" All interaction with
-- Sieve should ideally be through this function/operator. The most basic usage of it can be seen in the following example:
--
-- >  f3 =  [0,11,10] ++? ([7,8,9]  ++?  ([4,5,6] ++? newSieve (\x -> x > 2) [1,2,3]))
--
-- This produces the list @[11,10,7,8,9,4,5,6,3]@.
(++?) ::
  [a] -- ^ The list you wish to conditionally add to the 'Sieve'.
  -> Sieve a -- ^ The 'Sieve' to add elements to.
  -> Sieve a -- ^ The resulting 'Sieve' comprised of the contents of the old 'Sieve' combined with the elements from argument one that met the 'relation' criteria.
(++?) [] b = b
(++?) a b = Sieve rel $ Data.Sieve.filter rel a ++ toList  b
            where
              rel = relation b

-- | Similar Defn. From Prelude.
filter :: (a ->  Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:y)
  | p x  = x:Data.Sieve.filter p y 
  | otherwise = Data.Sieve.filter p  y

testSieve :: Sieve Int
testSieve = newSieve (\x -> x > 2) [1,2,3]



f2 :: Sieve Int -> Sieve Int
f2 s = [7,8,1] ++? s

f1 :: Sieve Int 
f1 = let numbersGreaterThanTwo = newSieve (\x -> x > 2) [1,2,3] in f2 $ [4,5,6] ++? numbersGreaterThanTwo

f3 :: Sieve Int
f3 =  [0,11,10] ++? ([7,8,9]  ++?  ([4,5,6] ++? newSieve (\x -> x > 2) [1,2,3]))
