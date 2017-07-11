{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib  where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


class Semiring m where
  _0 :: m
  -- _0 = mempty
  _1 :: m
  (<+>) :: m -> m -> m
  -- (<+>) = mappend
  (<.>) :: m -> m -> m

instance Semiring Bool where
  _0 = False
  _1 = True
  (<+>) = (||)
  (<.>) = (&&)


newtype Prob = Prob Double
  deriving (Num, Show)

instance Semiring Prob where
  _0 = Prob 0
  _1 = Prob 1
  (<+>) = (+)
  (<.>) = (*)


inf :: Double
inf = 1/0


newtype Log = Log Double
  deriving (Num, Show, Eq)

instance Semiring Log where
  _0 = Log inf
  _1 = Log 0
  Log x <+> Log y = Log (-log (exp (-x) + exp (-y)))
  (<.>) = (+)


newtype Tropical = Tropical Double
  deriving (Num, Show, Eq)

instance Semiring Tropical where
  _0 = Tropical inf
  _1 = Tropical 0
  Tropical x <+> Tropical y = Tropical (min x y)
  (<.>) = (+)

data Arc a b s where
  Arc :: (Semiring s, Show s, Eq s, Show a, Eq a, Show b, Eq b) =>
    { inputSymbol :: a, outputSymbol :: b, weight :: s } -> Arc a b s
deriving instance Eq (Arc a b s)
deriving instance Show (Arc a b s)


data WeightedFiniteState sid a b s
  = Source sid [(sid, Arc a b s)]
  | Mid sid [(sid, Arc a b s)]
  | Sink sid s [(sid, Arc a b s)]
  deriving (Show, Eq)

type Transducer sid a b s = [WeightedFiniteState sid a b s]


composeTransitions :: Semiring s => [(i, Arc a b s)] -> [(j, Arc b c s)] -> [((i, j), Arc a c s)]
composeTransitions atrs btrs = [((aid, bid), Arc ai bo (aw <.> bw))
                               | (aid, Arc ai ao aw) <- atrs,
                                 (bid, Arc bi bo bw) <- btrs,
                                 ao == bi]

composeStates :: Semiring s => WeightedFiniteState i a b s -> WeightedFiniteState j b c s -> WeightedFiniteState (i, j) a c s
composeStates (Source asid atrs)  (Source bsid btrs)  = Source (asid, bsid) $ composeTransitions atrs btrs
composeStates (Mid asid atrs)     (Mid bsid btrs)     = Mid    (asid, bsid) $ composeTransitions atrs btrs
composeStates (Sink asid aw atrs) (Sink bsid bw btrs) = Sink   (asid, bsid) (aw <.> bw) $ composeTransitions atrs btrs

compose :: Semiring s => Transducer aid a b s -> Transducer bid b c s -> Transducer (aid, bid) a c s
compose (Source  :as)  (b:bs) = iter (composeStates a b) abs
  where iter hd tl
-- ((Source aid atr) : atl) ((Source bid btr) : btl)  = [Source (aid, bid) []]


transA :: Transducer Int String String Tropical
transA = [
  Source 0
    [(1, Arc "a" "b" (Tropical 0.1)),
     (2, Arc "b" "c" (Tropical 0.2))],
  Mid 1
    [(1, Arc "c" "a" (Tropical 0.3)),
     (3, Arc "a" "a" (Tropical 0.4))],
  Mid 2
    [(3, Arc "b" "b" (Tropical 0.5))],
  Sink 3 (Tropical 0.6) []
  ]

transB :: Transducer Int String String Tropical
transB = [
  Source 0 [(1, Arc "b" "c" (Tropical 0.3))],
  Mid 1 [(2, Arc "a" "b" (Tropical 0.4))],
  Sink 2 (Tropical 0.7) [(2, Arc "a" "b" (Tropical 0.6))]
  ]


transC :: Transducer (Integer, Integer) String String Tropical
transC = [
  Source (0, 0) [((1, 1),  Arc "a" "c" (Tropical 0.4))],
  Mid (1, 1)
    [((1, 2), Arc "c" "b" (Tropical 0.7)),
     ((3, 2), Arc "a" "b" (Tropical 0.8))],
  Mid (1, 2)
    [((1, 2), Arc "c" "b" (Tropical 0.9)),
     ((3, 2), Arc "a" "b" (Tropical 1.0))],
  Sink (3, 2) (Tropical 1.3) []
  ]


minimize = undefined

determinize = undefined


funcc a b = a + b

arc = Arc "d" "data" (Log 3.2)
