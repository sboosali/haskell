{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.Data.Lens

import Control.Monad
import Control.Applicative
import Data.Map
-- 
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing
-- https://github.com/ekmett/lens/wiki/Examples


-- lenses are "functional references"
-- "a Lens focuses in on a smaller part of a larger object"
-- "JQuery for ADT's"


data Arc      = Arc      { _degree   :: Int, _minute    :: Int, _second :: Int } deriving (Show)
data Location = Location { _latitude :: Arc, _longitude :: Arc }                 deriving (Show)

-- makeLenses is Template Haskell
$(makeLenses ''Arc)
$(makeLenses ''Location)
{-

:t makeLenses
makeLenses :: Language.Haskell.TH.Syntax.Name
           -> Language.Haskell.TH.Syntax.Q [Language.Haskell.TH.Syntax.Dec]

the "$" means

the "''" means

-}


getLatitude :: Location -> Arc
getLatitude = view latitude 

setLatitude :: Arc -> Location -> Location
setLatitude = set latitude

modifyLatitude :: (Arc -> Arc) -> (Location -> Location)
modifyLatitude f = latitude `over` f
-- `over` lifts a getter/setter of a part to a modifier of the whole


getDegreeOfLat :: Location -> Int
getDegreeOfLat = view degree . view latitude

setDegreeOfLat' :: Int -> Location -> Location
setDegreeOfLat' = over latitude . set degree


latitude' :: Lens' Location Arc
latitude' = latitude

degree' :: Lens' Arc Int
degree' = degree 

degreeOfLat :: Lens' Location Int
degreeOfLat = latitude . degree

-- you compose Lenses with (.)
-- (just function composition from Prelude!)
-- it reads left-to-right like object-oriented chaining


setDegreeOfLat = set degreeOfLat

latitudeAndLongitude :: Lens' (Location, Location) (Arc, Arc)
latitudeAndLongitude = latitude `alongside` longitude

degreeOrMinute :: Lens' (Either Arc Arc) Int
degreeOrMinute = choosing degree minute

-- Lens' has kind 2
-- type Lens' s a = Lens s s a a

-- Lens has kind 4
-- it can change the types of the whole, when you change the type of a part
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)


degree'' :: Lens' Arc Int
degree'' = lens _degree (\arc int -> arc {_degree = int})
-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- lens combines a getter and a setter into a Lens
-- the setter can take a value of a different type, and change the type of the state


[2,3,4] = fmap (+1) [1,2,3]
[2,3,4] = (mapped `over` (+1)) [1,2,3]
{- ghci

:t (over mapped succ)
(over mapped succ) :: (Enum a, Functor f) => f a -> f a

:t (over mapped)
(over mapped) :: Functor f => (a -> b) -> f a -> f b

:i over
over :: Profunctor p => Setting p s t a b -> p a b -> s -> t
Control.Lens.Setter

-}


{- Lens laws

get and set are inverses
[1] view l (set l x o) == x
[2] set l (view l o) o == o

set is idempotent
[3] set l x (set l x o) == set l x o
    set l x . set l x   => set l x
where "l" is a lens, "x" is a value, "o" is a datum

-}



-- set the first item to hello
("hello", "world") =   set _1 "hello" ((), "world")
("hello", "world") =   ((), "world") & _1 .~ "hello"
-- python:             ((), "world")   [1] = "hello"

-- set the hello key to world
True = fromList [("hello","world")] ==   (set (at "hello") (Just "world") (fromList [("hello","there")]))
True = fromList [("hello","world")] ==   (fromList [("hello","there")]  &  at "hello"  ?~  "world")
-- python:                                          {"hello":"there"}        ["hello"]  =  "world"

-- increment both items by one
(2, 3) =   (both `over` (+1)) (1,2)
(2, 3) =   (1,2) & both +~ 1
-- python: (1,2)        += (1,1)


 {- 
_1             %~ succ $ (0, ("hello","world"))
_2.both.mapped %~ succ $ (0, ("hello","world"))
--ERROR both.mapped %~ succ $ ([0::Int], "o")
--ERROR both.(to length) %~ succ $ ("hello","world")


-} 

reflect'' (Arc d m s) = Arc (-d) m s           -- pattern matching syntax
reflect' arc = arc { _degree = -(_degree arc)} -- record update syntax
reflect = degree `over` (*(-1))                -- lenses

sanFrancisco = Location (Arc 37 45 36) (Arc 122 26 15)


main = do

 print $ sanFrancisco
 putStrLn ""

 print $ view latitudeAndLongitude (sanFrancisco,sanFrancisco)
 print $ view degreeOrMinute $ Left  (Arc 37 45 36)
 print $ view degreeOrMinute $ Right (Arc 37 45 36)
 putStrLn ""

 print $ view (latitude.to reflect.degree.to succ) sanFrancisco
 -- `to` lifts a pure function to a Getter
 {- ghci

 :i to
 to :: (s -> a) -> IndexPreservingGetter s a
 Control.Lens.Getter

 -}
 putStrLn ""

 print $ (latitudeAndLongitude.both.degree) `over` succ $ (sanFrancisco,sanFrancisco)
 --    latitudeAndLongitude                   both                        degree
 -- :: Lens' (Location, Location) (Arc, Arc)  Traversal' (Arc, Arc) Arc   Lens' Arc Int
 -- to help with the data flow: Traversal' doesn't exist and I instantiated the Bitraversable with (,)
 {- ghci

 let both' :: Traversal (Arc, Arc) (Arc, Arc) Arc Arc; both' = both in (latitudeAndLongitude.both'.degree) `over` succ $ (sanFrancisco,sanFrancisco)

 -}
 putStrLn ""

-- Fold is like Foldable  
 print $ anyOf biplate (=="world") ("hello", (), [(2::Int,"world")])
 putStrLn ""

-- Traversal is like Traversable
 print =<< mapMOf (traverse._2) (\xs -> length xs <$ print xs) [(1,"hello"),(2::Int,"world")]
 print $   over   (mapped  ._2) (\xs -> length xs)             [(1,"hello"),(2::Int,"world")]
-- the traverse enters the list, with effects, and the _2 enters the pair
-- the mapped   enters the list,               and the _2 enters the pair
 putStrLn ""

