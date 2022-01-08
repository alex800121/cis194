import Data.List
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.List

{-
type HanoiTower = StateT [[Peg]] (WriterT [Move] []) 

moveOne :: [[Peg]] -> Move -> [[Peg]]
moveOne s (a,b)
  | s == [] = []
  | s !! a == [] = []
  | s !! b == [] = f
  | head ( s !! a ) > head ( s !! b ) = []
  | otherwise = f
      where
        newA = tail ( s !! a )
        newB = head ( s !! a ) : ( s !! b )
        tempA = take a s ++ newA : drop (a+1) s
        f = take b tempA ++ newB : drop (b+1) tempA

moves :: Int -> [Move]
moves n = [ (x,y) | x <- [0..(n-1)] , y <- [0..(n-1)] , x /= y ]
-}


type Peg = String
type Move = (Peg,Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ (a,b) : hanoi (n-1) c b a

hanoiPlus :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiPlus n a b c t =
  let
    nList = take' 1 n $ []
    f [] a' b' c' t' = [] 
    f (x:xs) a' b' c' t' =
      f xs a' c' b' t'
      ++  hanoi x a' b' t'
      ++  f xs c' b' a' t'                    
  in f nList a b c t


take' n x ls =
  case compare n x of
    GT  -> zipWith (+) (replicate x 1 ++ repeat 0) ls
    LT  -> take' (n + 1) (x - n) (n : ls)
    EQ  -> n : ls
