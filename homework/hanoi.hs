import Data.List
import Control.Monad
import qualified Data.Set as S

type Peg = String
type Move = (Peg,Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ (a,b) : hanoi (n-1) c b a
