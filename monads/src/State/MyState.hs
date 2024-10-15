{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
module State.MyState where 

newtype MyState s a = MyState { runMyState :: s -> (s, a) }

get :: MyState s s 
get = undefined

gets :: (s -> a) -> MyState s a 
gets f = undefined

put :: s -> MyState s () 
put s = undefined

modify :: (s -> s) -> MyState s () 
modify f = undefined

instance Functor (MyState s) where 
  fmap f (MyState g) = undefined

instance Applicative (MyState s) where 
  pure x = undefined

  (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b 
  MyState f <*> MyState x = undefined
 
instance Monad (MyState s) where 
  MyState f >>= m = undefined