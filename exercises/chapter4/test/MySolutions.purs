module Test.MySolutions where

import Prelude
import Test.Examples

import Control.Alternative (guard)
import Data.Array (filter, foldl, foldMap, fromFoldable, head, last, length, mapMaybe, sortWith, (..))
import Data.Array.NonEmpty as NE
import Data.Foldable (and)
import Data.Int (rem)
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid.Additive (Additive(Additive))
import Data.Path (Path, filename, isDirectory, ls, size)
import Data.String (contains, Pattern(Pattern))

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven i = rem i 2 == 0

countEven :: Array Int -> Int
countEven a = r
 where
  onEven i = if isEven i then one else zero
  Additive r = foldMap (onEven >>> Additive) a

squared :: forall f. Functor f => f Number -> f Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (zero <= _)

infixr 5 filter as <$?>

keepNonNegativeRewrite :: forall a. Semiring a => Ord a => Array a -> Array a
keepNonNegativeRewrite = ((zero <= _) <$?> _)

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct as bs = do
  a <- as
  b <- bs
  pure [a, b]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard (a * a + b * b == c * c)
  pure [a, b, c]

primeFactors :: Int -> Array Int
primeFactors n = fromFoldable (pf n)
 where
  pf 1 = Nil
  pf c = pf' 2
   where
    pf' f | c <= f = Cons c Nil
    pf' f | c `rem` f == 0 = Cons f (pf (c / f))
    pf' f = pf' (one + f)

allTrue :: Array Boolean -> Boolean
allTrue = and

-- `foldl (==) false xs` returns `true` when `xs` contains an odd number of
-- `false` values

fibTailRec :: Int -> Int
fibTailRec = ftr 0 1
 where
  ftr c _ 0 = c
  ftr _ n 1 = n
  ftr c n i = let nn = n + c in ftr nn (nn + n) (i - 2)

reverse :: forall a. Array a -> Array a
reverse a = foldl alg identity a []
 where
  alg f x xs = f (xs `append` [x])

onlyFiles :: Path -> Array Path
onlyFiles = filter (not <<< isDirectory) <<< allFiles

whereIs :: Path -> String -> Maybe Path
whereIs r n = head $ do
  dent <- ls r
  if isDirectory dent
    then maybe [] pure (whereIs dent n)
    else if contains (Pattern n) (filename dent)
           then pure r
           else []

largestSmallest :: Path -> Array Path
largestSmallest d =
  case sized of
      Nothing -> []
      Just nesz ->
        if NE.length nesz == 1
          then [(NE.head nesz).path]
          else
            let sorted = NE.sortWith (_.size) nesz
             in [(NE.last sorted).path, (NE.head sorted).path]
 where
  sized = NE.fromArray <<< mapMaybe wSize $ allFiles d
  wSize p = map (\s -> { size: s, path: p }) $ size p
