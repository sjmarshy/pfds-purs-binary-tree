module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data NumberTree
  = Empty 
  | Node 
    { left :: NumberTree
    , el :: Int
    , right :: NumberTree
    }

instance treeShow :: Show NumberTree where
  show Empty = "Empty"
  show (Node { left: left, el: el, right: right }) = "Node( " <> show left <> " " <> show el <> " " <> show right <> " )"

empty :: NumberTree -> Boolean
empty Empty = true
empty _ = false

raw :: NumberTree -> Int -> NumberTree -> NumberTree
raw left el right = Node { left: left, el: el, right: right }

insert :: Int -> NumberTree -> NumberTree
insert n Empty = raw Empty n Empty
insert n (Node { left: left, el: el, right: right }) =
  if n < el then raw (insert n left) el right else raw left el (insert n right)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ insert 10 $ insert 8 Empty
