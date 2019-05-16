{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if HAVE_QUANTIFIED_CONSTRAINTS
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-# OPTIONS_GHC -Wall #-}

module Test.QuickCheck.Classes.Foldable
  (
#if HAVE_UNARY_LAWS
    foldableLaws
#endif
  ) where

import Data.Monoid
import Data.Foldable
import Test.QuickCheck hiding ((.&.))
import Control.Exception (ErrorCall,try,evaluate)
import Control.Monad.Trans.Class (lift)
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Arbitrary (Arbitrary1(..))
#endif
import Test.QuickCheck.Monadic (monadicIO)
#if HAVE_UNARY_LAWS
import Data.Functor.Classes (Eq1,Show1)
#endif
import Test.QuickCheck.Property (Property)

import qualified Data.Foldable as F
import qualified Data.Semigroup as SG

import Test.QuickCheck.Classes.Common
#if HAVE_UNARY_LAWS
import Test.QuickCheck.Classes.Compat (eq1)
#endif

#if HAVE_UNARY_LAWS

-- | Tests the following 'Foldable' properties:
--
-- [/fold/]
--   @'fold' ≡ 'foldMap' 'id'@
-- [/foldMap/]
--   @'foldMap' f ≡ 'foldr' ('mappend' . f) 'mempty'@
-- [/foldr/]
--   @'foldr' f z t ≡ 'appEndo' ('foldMap' ('Endo' . f) t ) z@
-- [/foldr'/]
--   @'foldr'' f z0 xs ≡ let f\' k x z = k '$!' f x z in 'foldl' f\' 'id' xs z0@
-- [/foldr1/]
--   @'foldr1' f t ≡ let 'Just' (xs,x) = 'unsnoc' ('toList' t) in 'foldr' f x xs@
-- [/foldl/]
--   @'foldl' f z t ≡ 'appEndo' ('getDual' ('foldMap' ('Dual' . 'Endo' . 'flip' f) t)) z@
-- [/foldl'/]
--   @'foldl'' f z0 xs ≡ let f' x k z = k '$!' f z x in 'foldr' f\' 'id' xs z0@
-- [/foldl1/]
--   @'foldl1' f t ≡ let x : xs = 'toList' t in 'foldl' f x xs@
-- [/toList/]
--   @'F.toList' ≡ 'foldr' (:) []@
-- [/null/]
--   @'null' ≡ 'foldr' ('const' ('const' 'False')) 'True'@
-- [/length/]
--   @'length' ≡ 'getSum' . 'foldMap' ('const' ('Sum' 1))@
--
-- Note that this checks to ensure that @foldl\'@ and @foldr\'@
-- are suitably strict.
foldableLaws :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Foldable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Foldable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
foldableLaws = foldableLawsInternal

foldableLawsInternal :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Foldable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Foldable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Laws
foldableLawsInternal p = Laws "Foldable"
  [ (,) "fold" $ property $ \(Apply (a :: f (SG.Sum Integer))) ->
      F.fold a == F.foldMap id a
  , (,) "foldMap" $ property $ \(Apply (a :: f Integer)) (e :: QuadraticEquation) ->
      let f = SG.Sum . runQuadraticEquation e
       in F.foldMap f a == F.foldr (mappend . f) mempty a
  , (,) "foldr" $ property $ \(e :: LinearEquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runLinearEquationTwo e
       in F.foldr f z t == SG.appEndo (foldMap (SG.Endo . f) t) z
  , (,) "foldr'" (foldableFoldr' p)
  , (,) "foldl" $ property $ \(e :: LinearEquationTwo) (z :: Integer) (Apply (t :: f Integer)) ->
      let f = runLinearEquationTwo e
       in F.foldl f z t == SG.appEndo (SG.getDual (F.foldMap (SG.Dual . SG.Endo . flip f) t)) z
  , (,) "foldl'" (foldableFoldl' p)
  , (,) "foldl1" $ property $ \(e :: LinearEquationTwo) (Apply (t :: f Integer)) ->
      case compatToList t of
        [] -> True
        x : xs ->
          let f = runLinearEquationTwo e
           in F.foldl1 f t == F.foldl f x xs
  , (,) "foldr1" $ property $ \(e :: LinearEquationTwo) (Apply (t :: f Integer)) ->
      case unsnoc (compatToList t) of
        Nothing -> True
        Just (xs,x) ->
          let f = runLinearEquationTwo e
           in F.foldr1 f t == F.foldr f x xs
  , (,) "toList" $ property $ \(Apply (t :: f Integer)) ->
      eq1 (F.toList t) (F.foldr (:) [] t)
#if MIN_VERSION_base(4,8,0)
  , (,) "null" $ property $ \(Apply (t :: f Integer)) ->
      null t == F.foldr (const (const False)) True t
  , (,) "length" $ property $ \(Apply (t :: f Integer)) ->
      F.length t == SG.getSum (F.foldMap (const (SG.Sum 1)) t)
#endif
  ]

unsnoc :: [a] -> Maybe ([a],a)
unsnoc [] = Nothing
unsnoc [x] = Just ([],x)
unsnoc (x:y:xs) = fmap (\(bs,b) -> (x:bs,b)) (unsnoc (y : xs))

compatToList :: Foldable f => f a -> [a]
compatToList = foldMap (\x -> [x])

foldableFoldl' :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Foldable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Foldable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
foldableFoldl' _ = property $ \(_ :: ChooseSecond) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Integer -> Bottom Integer -> Integer
        f a b = case b of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then a
            else v
        z0 = 0
    r1 <- lift $ do
      let f' x k z = k $! f z x
      e <- try (evaluate (F.foldr f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldl' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

foldableFoldr' :: forall proxy f.
#if HAVE_QUANTIFIED_CONSTRAINTS
  (Foldable f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
#else
  (Foldable f, Eq1 f, Show1 f, Arbitrary1 f)
#endif
  => proxy f -> Property
foldableFoldr' _ = property $ \(_ :: ChooseFirst) (_ :: LastNothing) (Apply (xs :: f (Bottom Integer))) ->
  monadicIO $ do
    let f :: Bottom Integer -> Integer -> Integer
        f a b = case a of
          BottomUndefined -> error "foldableFoldl' example"
          BottomValue v -> if even v
            then v
            else b
        z0 = 0
    r1 <- lift $ do
      let f' k x z = k $! f x z
      e <- try (evaluate (F.foldl f' id xs z0))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    r2 <- lift $ do
      e <- try (evaluate (F.foldr' f z0 xs))
      case e of
        Left (_ :: ErrorCall) -> return Nothing
        Right i -> return (Just i)
    return (r1 == r2)

#endif
