{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module DBSP.Stream (
  Stream(..),
  delay,
  differentiate,
  integrate,
  Circuit(..),
  differentiateC,
  integrateC,
  zipStreams
) where

import Data.Monoid
import Data.Group
import Control.Arrow
import qualified Control.Category as Cat
import Control.Category (Category(..))
import Data.List (scanl', zipWith)

newtype Stream a = Stream { unStream :: [a] }
  deriving (Show, Functor)

instance Monoid a => Monoid (Stream a) where
  mempty = Stream []
  
instance Semigroup a => Semigroup (Stream a) where
  (<>) = zipStreams (<>) 

instance Group a => Group (Stream a) where
  invert = fmap invert
  (~~) = zipStreams (~~)

delay :: Monoid a => Stream a -> Stream a
delay (Stream []) = Stream []
delay (Stream xs) = Stream $ mempty : xs

differentiate :: Group a => Stream a -> Stream a
differentiate s = zipStreams (<>) s (delay $ fmap invert s)

integrate :: Monoid a => Stream a -> Stream a
integrate = Stream Cat.. tail Cat.. scanl' (<>) mempty Cat.. unStream


zipStreams :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipStreams f (Stream a) (Stream b) = Stream $ zipWith f a b

newtype Circuit a b = Circuit { runCircuit :: [a] -> [b] }

instance Category Circuit where
  id = Circuit Cat.id
  Circuit f . Circuit g = Circuit (f Cat.. g)

instance Arrow Circuit where
  arr f = Circuit (map f)
  first (Circuit f) = Circuit $ \input ->
    let (bs, ds) = unzip input 
        cs = f bs              
    in zip cs ds      

differentiateC :: Group a => Circuit a a
differentiateC = Circuit $ \xs ->
  zipWith (~~) xs (mempty : xs)

integrateC :: Monoid a => Circuit a a
integrateC = Circuit (scanl' mappend mempty)
