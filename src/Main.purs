module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))

data Element a = Element a
data NumberTree a
  = Empty 
  | Node (NumberTree a) (Element a) (NumberTree a)

instance showElement :: (Show a) => Show (Element a) where
  show (Element a) = show a

instance showNumberTree :: (Show a) => Show (NumberTree a) where
  show Empty = "Empty"
  show (Node left el right) = "Node( " <> show left <> " " <> show el <> " " <> show right <> " )"

empty :: forall a. NumberTree a -> Boolean
empty Empty = true
empty _ = false

raw :: forall a. NumberTree a -> a -> NumberTree a -> NumberTree a
raw left el right = Node left (Element el) right

member :: forall a. (Ord a) => NumberTree a -> a -> Maybe a -> Boolean
member Empty _ _ = false
member (Node Empty (Element el) Empty) mem (Just test) = mem == el || test == el
member (Node Empty (Element el) Empty) mem Nothing = mem == el
member (Node left (Element el) right) mem t =
  if mem < el
  then member left mem t
  else member right mem (Just el)

insert :: forall a. (Ord a) => a -> NumberTree a -> NumberTree a
insert n Empty = raw Empty n Empty
insert n (Node left (Element el) right) =
  if n < el 
  then raw (insert n left) el right 
  else raw left el (insert n right)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ insert 10 $ insert 8 Empty
  log $ show $ member (insert 10 $ insert 9 $ insert 8 Empty) 10 Nothing
  
