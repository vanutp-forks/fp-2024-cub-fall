{-# LANGUAGE InstanceSigs #-}
module Reader.MyReader where 

newtype MyReader r a = MyReader { runMyReader :: r -> a }

ask :: MyReader r r 
ask = undefined 

local :: (r -> r) -> MyReader r a -> MyReader r a 
local f (MyReader g) = undefined 

instance Functor (MyReader r) where 
  fmap :: (a -> b) -> MyReader r a -> MyReader r b 
  fmap f (MyReader g) = undefined 

instance Applicative (MyReader r) where 
  pure :: a -> MyReader r a 
  pure = undefined 

  (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
  MyReader f <*> MyReader x = undefined

instance Monad (MyReader r) where 
  (>>=) :: MyReader r a -> (a -> MyReader r b) -> MyReader r b
  MyReader x >>= m = undefined
