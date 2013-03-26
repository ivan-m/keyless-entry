{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
   Module      : GraphBench
   Description : Graph-like benchmarks for the keyless-entry library
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Main where


import Prelude hiding (lookup, map)
import Data.Keyless
import qualified Data.Keyless.IntMap.Lazy as IL
import qualified Data.Keyless.Map.Lazy as ML
import qualified Data.Keyless.Vector as V

import Criterion
import Criterion.Main(defaultMainWith)
import Criterion.Config(Config(..), defaultConfig, ljust)
import Control.DeepSeq

import Data.Functor((<$>))
import qualified Data.IntMap.Strict as IM
import Data.Maybe(catMaybes, listToMaybe, fromMaybe)
import Data.Monoid(Monoid(..))
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (|>), viewl, ViewL(..))
import qualified Data.Traversable as T
import Control.Arrow((***), (&&&), second)
import Control.Exception(evaluate)
import Control.Monad.State

-- -----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWith myConfig
       (liftIO . evaluate $ rnf [ B lazyMapKL
                                , B lazyMapKLI
                                , B lazyIntMapKL
                                , B lazyIntMapKLI
                                , B vectorKL
                                , B vectorKLI
                                ])
       [ mkBenchGroup "traversal" benchTrav
       , mkBenchGroup "traversal-conversion" ((fmap K .) . benchTravConv)
       , mkBenchGroup' "traversal-merge-conversion" benchTravMergeConv
       , mkBenchGroup' "traversal-conversion-merge" benchTravConvMerge
       ]

myConfig :: Config
myConfig = defaultConfig { cfgReport = ljust "keylessGraph.html" }

mkBenchGroup' :: String -> (forall c. (GKeyless c) => Bool -> Graph c -> Graph c)
                 -> Benchmark
mkBenchGroup' title f = mkBenchGroup title $ (K .) . f

mkBenchGroup :: (NFData r) => String
                -> (forall c. (GKeyless c) => Bool -> Graph c -> r) -> Benchmark
mkBenchGroup title f = bgroup title [ mkBG "sequential" seqs
                                    , mkBG "intermingled" inter
                                    ]
  where
    mkBG ttl gs = bgroup ttl [ bgroup "internal lookup" $ fmap (toBnch False) gs
                             , bgroup "external lookup" $ fmap (toBnch True) gs
                             ]

    toBnch ex (nm,k) = bench nm . appK (nf $ f ex) $ k

    seqs = [ ("Map.Lazy", K lazyMapKL)
           , ("IntMap.Lazy", K lazyIntMapKL)
           , ("Vector", K vectorKL)
           ]

    inter = [ ("Map.Lazy", K lazyMapKLI)
            , ("IntMap.Lazy", K lazyIntMapKLI)
            , ("Vector", K vectorKLI)
            ]

data K where
  K :: (GKeyless c) => Graph c -> K

instance NFData K where
    rnf (K g) = rnf g

appK :: (forall c. (GKeyless c) => Graph c -> a) -> K -> a
appK f (K c) = f c

-- -----------------------------------------------------------------------------

blankNew :: Key
blankNew = -1

isBlank :: Key -> Bool
isBlank = (<0)

data NInfo = NI { firstEdge :: !Edge
                , newNID    :: !Node
                }
             deriving (Eq, Ord, Show, Read)

setNID :: Node -> NInfo -> NInfo
setNID n' ni = ni { newNID = n' }

data EInfo = EI { next   :: !Edge
                , prev   :: !Edge
                , inv    :: !Edge
                , from   :: !Node
                , to     :: !Node
                , newEID :: !Edge
                }
           deriving (Eq, Ord, Show, Read)

setEID :: Node -> EInfo -> EInfo
setEID e' ei = ei { newEID = e' }

instance NFData NInfo where
  rnf (NI fe nid) = rnf fe `seq` rnf nid

instance NFData EInfo where
  rnf (EI n p i f t nid) = rnf n `seq` rnf p `seq` rnf i `seq` rnf f `seq` rnf t `seq` rnf nid

type Edge = Key
type Node = Key

data Graph c = G { nodes :: !(c NInfo)
                 , edges :: !(c EInfo)
                 }

adjNodes :: (c NInfo -> c NInfo) -> Graph c -> Graph c
adjNodes fn g = g { nodes = fn $ nodes g }

adjEdges :: (c EInfo -> c EInfo) -> Graph c -> Graph c
adjEdges fe g = g { edges = fe $ edges g }

type GKeyless c = (Keyless c, NFData (c NInfo), NFData (c EInfo))

instance (GKeyless c) => NFData (Graph c) where
  rnf (G ns es) = rnf ns `seq` rnf es

mergeAllGraphs :: (Keyless c) => [Graph c] -> Graph c
mergeAllGraphs gs = G ns' es'
  where
    (nss, ess) = unzip $ fmap (nodes &&& edges) gs

    (nmts, ns) = mergeAll nss
    (emts, es) = mergeAll ess

    nTrans = catMaybes $ zipWith mkNT nmts emts
    mkNT nmt emt = (,fni) . snd <$> newBounds nmt
      where
        fni ni = ni { firstEdge = oldToNew emt $ firstEdge ni
                    }

    eTrans = catMaybes $ zipWith mkET emts nmts
    mkET emt nmt = (,fei) . snd <$> newBounds emt
      where
        fei ei = ei { next   = oldToNew emt $ next   ei
                    , prev   = oldToNew emt $ prev   ei
                    , inv    = oldToNew emt $ inv    ei
                    , from   = oldToNew nmt $ from   ei
                    , to     = oldToNew nmt $ to     ei
                    }

    ns' = mapWithKey fn ns
      where
        fn n ni = maybe ni (($ni) . snd) . listToMaybe
                  $ dropWhile ((<n) . fst) nTrans

    es' = mapWithKey fe es
      where
        fe e ei = maybe ei (($ei) . snd) . listToMaybe
                  $ dropWhile ((<e) . fst) eTrans

{-

The graph we're going to use is:

   >            0/1        2/3
   >        0---------1---------2
   >        |         |         |
   >    4/5 |     6/7 |     8/9 |
   >        |  10/11  |   12/13 |
   >        3---------4---------5
   >         \                 /
   >          \               /
   >           \             /
   >     14/15  \           / 16/17
   >             \         /
   >              \       /
   >               \     /
   >                \   /
   >                 \ /
   >                  6


-}

data ListGraph = LG { lnodes :: ![(Node, NInfo)]
                    , ledges :: ![(Edge, EInfo)]
                    }
               deriving (Eq, Ord, Show, Read)

toGraph :: (Keyless c) => ListGraph -> Graph c
toGraph (LG lns les) = G { nodes = unsafeFromListWithKeys lns
                         , edges = unsafeFromListWithKeys les
                         }

-- | mappend and mconcat assume they don't have common nodes or edges
instance Monoid ListGraph where
  mempty = LG [] []

  mappend (LG ln1 le1) (LG ln2 le2) = LG (ln1 ++ ln2) (le1 ++ le2)

initListGraph :: ListGraph
initListGraph = LG nodesL edgesL

-- [(0,NI 0 blankNew), (1, NI 1 blankNew)] [(0,EI 0 0 1 0 1 blankNew), (1, EI 1 1 0 1 0 blankNew)] --

maxNodeLG :: Node
maxNodeLG = maximum $ nsLst ++ fnLst ++ tnLst
  where
    nsLst = fst <$> lnodes initListGraph
    fnLst = (from . snd) <$> ledges initListGraph
    tnLst = (to . snd) <$> ledges initListGraph

maxEdgeLG :: Edge
maxEdgeLG = maximum . filter even $ neLst ++ esLst ++ nxtLst ++ prvLst ++ invLst
  where
    neLst = firstEdge . snd <$> lnodes initListGraph
    esLst = fst <$> ledges initListGraph
    nxtLst = (next . snd) <$> ledges initListGraph
    prvLst = (prev . snd) <$> ledges initListGraph
    invLst = (inv . snd) <$> ledges initListGraph

nodesL :: [(Node, NInfo)]
nodesL = [ (0,NI 0 blankNew)
         , (1,NI 1 blankNew)
         , (2,NI 3 blankNew)
         , (3,NI 5 blankNew)
         , (4,NI 7 blankNew)
         , (5,NI 9 blankNew)
         , (6,NI 15 blankNew)
         ]

edgesL :: [(Edge, EInfo)]
edgesL = [ (0,  EI { next = 4,  prev = 4,  inv = 1,  from = 0, to = 1, newEID = blankNew })
         , (1,  EI { next = 2,  prev = 6,  inv = 0,  from = 1, to = 0, newEID = blankNew })
         , (2,  EI { next = 6,  prev = 1,  inv = 3,  from = 1, to = 2, newEID = blankNew })
         , (3,  EI { next = 8,  prev = 8,  inv = 2,  from = 2, to = 1, newEID = blankNew })
         , (4,  EI { next = 0,  prev = 0,  inv = 5,  from = 0, to = 3, newEID = blankNew })
         , (5,  EI { next = 10, prev = 14, inv = 4,  from = 3, to = 0, newEID = blankNew })
         , (6,  EI { next = 1,  prev = 2,  inv = 7,  from = 1, to = 4, newEID = blankNew })
         , (7,  EI { next = 12, prev = 11, inv = 6,  from = 4, to = 1, newEID = blankNew })
         , (8,  EI { next = 3,  prev = 3,  inv = 9,  from = 2, to = 5, newEID = blankNew })
         , (9,  EI { next = 16, prev = 13, inv = 8,  from = 5, to = 2, newEID = blankNew })
         , (10, EI { next = 14, prev = 5,  inv = 11, from = 3, to = 4, newEID = blankNew })
         , (11, EI { next = 7,  prev = 12, inv = 10, from = 4, to = 3, newEID = blankNew })
         , (12, EI { next = 11, prev = 7,  inv = 13, from = 4, to = 5, newEID = blankNew })
         , (13, EI { next = 9,  prev = 16, inv = 12, from = 5, to = 4, newEID = blankNew })
         , (14, EI { next = 5,  prev = 10, inv = 15, from = 3, to = 6, newEID = blankNew })
         , (15, EI { next = 17, prev = 17, inv = 14, from = 6, to = 3, newEID = blankNew })
         , (16, EI { next = 13, prev = 9,  inv = 17, from = 5, to = 6, newEID = blankNew })
         , (17, EI { next = 15, prev = 15, inv = 16, from = 6, to = 5, newEID = blankNew })
         ]

fGraph :: ListGraph -> (Node -> Node) -> (Edge -> Edge) -> ListGraph
fGraph lg fn fe = LG { lnodes = (fn  *** fNI) <$> lnodes lg
                     , ledges = (fe' *** fEI) <$> ledges lg
                     }
  where
    fe' e | even e    = fe e
          | otherwise = succ . fe $ pred e

    fNI ni = ni { firstEdge = fe' $ firstEdge ni }

    fEI ei = ei { next = fe' $ next ei
                , prev = fe' $ prev ei
                , inv  = fe' $ inv  ei
                , from = fn  $ from ei
                , to   = fn  $ to   ei
                }

multipleListGraph :: Int -> ListGraph
multipleListGraph = mconcat . fmap (liftM2 (fGraph initListGraph) fn fe)
                    . enumFromTo 0 . pred
  where
    fn x = (+) (x*(maxNodeLG+1))
    fe x = (+) (x*(maxEdgeLG+2))

multipleIntermingledListGraph   :: Int -> ListGraph
multipleIntermingledListGraph c = mconcat . fmap (liftM2 (fGraph initListGraph) fn fe)
                                  . enumFromTo 0 $ pred c
  where
    fn x = (x+) . (c*)
    -- Wanting odd = even+1 for invs
    fe x = (+) (2*x) . (c*)

-- -----------------------------------------------------------------------------

type SerialisedGraph = [SerialisedNode]

type SerialisedNode = (Node, [SerialisedEdge])

type SerialisedEdge = (Edge,Node, Edge)

deserialise :: (Keyless c) => SerialisedGraph -> Graph c
deserialise = uncurry G . (unsafeFromListWithKeys *** unsafeFromListWithKeys)
              . second concat . unzip . fmap deserialiseNode

deserialiseNode :: SerialisedNode -> ((Node,NInfo), [(Edge,EInfo)])
deserialiseNode (n,es) = ((n,ni), zipWith3 mkE prvEs es nxtEs)
  where
    ni = NI (getE $ head es) blankNew

    getE (e,_,_) = e

    es' = getE <$> es
    nxtEs = tail $ cycle es'
    prvEs = cycle . uncurry (:) $ splitLast es'

    mkE prv (e,t,ei) nxt = (e, EI nxt prv ei n t blankNew)

splitLast :: [a] -> (a,[a])
splitLast [] = error "splitLast"
splitLast [a] = (a,[])
splitLast (a:as) = second (a:) $ splitLast as

-- -----------------------------------------------------------------------------

type TravState c = State (TState c)

data TState c = TS { graphState :: !(Graph c)
                   , externalLk :: !Bool
                   , toVisit    :: !(Seq Node)
                   , nextNodeID :: !Node
                   , nLookup    :: !(IM.IntMap Node)
                   , nextEdgeID :: !Node
                   , eLookup    :: !(IM.IntMap Edge)
                   , separate   :: !Bool
                   , travNodes  :: ![Node]
                   , travEdges  :: ![Edge]
                   }

traverse        :: (Keyless c) => Bool -> Bool -> Graph c -> [SerialisedGraph]
traverse s ex g = evalState traverseAll $ initFrom s ex g

initFrom       :: Bool -> Bool -> Graph c -> TState c
initFrom s e g = TS { graphState = g
                    , externalLk = e
                    , toVisit    = Seq.empty
                    , nextNodeID = initKey
                    , nLookup    = IM.empty
                    , nextEdgeID = initKey
                    , eLookup    = IM.empty
                    , separate   = s
                    , travNodes  = []
                    , travEdges  = []
                    }

traverseAll :: (Keyless c) => TravState c [SerialisedGraph]
traverseAll = unfoldrM traverseComponent

traverseComponent :: (Keyless c) => TravState c (Maybe SerialisedGraph)
traverseComponent = gets (minKey . nodes . graphState)
                    >>= T.mapM traverseComponentFrom

traverseComponentFrom   :: (Keyless c) => Node -> TravState c SerialisedGraph
traverseComponentFrom n = do modify $ \ ts -> ts { toVisit   = Seq.singleton n }
                             sg <- unfoldrM trv
                             (tns,tes) <- gets $ (travNodes &&& travEdges)
                             modify $ \ ts ->
                               strtComp ts { graphState = adjEdges (deleteBulk tes)
                                                          . adjNodes (deleteBulk tns)
                                                          $ graphState ts
                                           , nLookup    = IM.empty
                                           , eLookup    = IM.empty
                                           , travNodes  = []
                                           , travEdges  = []
                                           }
                             return sg
  where
    trv = nextNode >>= T.mapM traverseNode

    strtComp ts
      | separate ts = ts { nextNodeID = initKey, nextEdgeID = initKey }
      | otherwise   = ts

nextNode :: TravState c (Maybe Node)
nextNode = do mv <- gets toVisit
              case viewl mv of
                EmptyL  -> return Nothing
                n :< v' -> do modify $ \ ts -> ts { toVisit = v' }
                              return $ Just n

traverseNode :: (Keyless c) => Node -> TravState c SerialisedNode
traverseNode n = do n' <- getNewNID n
                    e  <- gets $ firstEdge . unsafeLookup n . nodes . graphState
                    ((,) n') <$> traverseNodeEdges e

traverseNodeEdges :: (Keyless c) => Edge -> TravState c [SerialisedEdge]
traverseNodeEdges e = unfoldWhileM (/=e) traverseNodeEdge e

traverseNodeEdge :: (Keyless c) => Edge -> TravState c (Edge,SerialisedEdge)
traverseNodeEdge e = do (n,t) <- gets $ (next &&& to) . unsafeLookup e . edges . graphState
                        (e',ei') <- getNewEID e
                        t' <- getNewNID t
                        return (n,(e',t',ei'))

getNewNID :: (Keyless c) => Node -> TravState c Node
getNewNID n = do ex <- gets externalLk
                 n' <- gets $ if ex
                              then fromMaybe blankNew . IM.lookup n . nLookup
                              else newNID . unsafeLookup n . nodes . graphState
                 if isBlank n'
                   then do nextN <- gets nextNodeID
                           modify $ setNew ex nextN
                           return nextN
                   else return n'
  where
    setNew ex n' ts = ts' { toVisit    = toVisit ts' |> n
                          , nextNodeID = succ n'
                          , travNodes  = n : travNodes ts'
                          }
      where
        ts' | ex        = ts { nLookup = IM.insert n n' $ nLookup ts }
            | otherwise = ts { graphState = adjNodes (adjust (setNID n') n) $ graphState ts }

getNewEID :: (Keyless c) => Edge -> TravState c (Edge, Edge)
getNewEID e = do (ex, ei) <- gets $ (externalLk &&& inv . unsafeLookup e . edges . graphState)
                 e' <- gets $ if ex
                              then fromMaybe blankNew . IM.lookup e . eLookup
                              else newEID . unsafeLookup e . edges . graphState
                 if isBlank e'
                   then do nextE <- gets nextEdgeID
                           let nextE' = succ nextE
                           modify $ setNew ex ei nextE nextE'
                           return (nextE, nextE')
                   else ((,) e') <$> gets (getInv ex ei)
  where
    setNew ex ei e' ei' ts = ts' { nextEdgeID = succ ei'
                                 , travEdges  = e : ei : travEdges ts'
                                 }
      where
        ts' | ex        = ts { eLookup = IM.insert ei ei' . IM.insert e e' $ eLookup ts }
            | otherwise = ts { graphState = adjEdges (adjust (setEID ei') ei . adjust (setEID e') e)
                                            $ graphState ts
                             }

    getInv ex ei
      | ex        = (IM.! ei) . eLookup
      | otherwise = newEID . unsafeLookup ei . edges . graphState

-- Initial value _not_ tested!
unfoldWhileM :: (Functor m, Monad m) => (a -> Bool) -> (a -> m (a,b)) -> a -> m [b]
unfoldWhileM p f = go
  where
    go a = do (a',b) <- f a
              if p a'
                then (b:) <$> go a'
                else return [b]

unfoldrM    :: (Functor m, Monad m) => m (Maybe a) -> m [a]
unfoldrM mf = go =<< mf
  where
    go Nothing  = return []
    go (Just a) = fmap (a:) . go =<< mf

-- -----------------------------------------------------------------------------

benchTrav :: (Keyless c) => Bool -> Graph c -> [SerialisedGraph]
benchTrav = traverse True

benchTravConv :: (Keyless c) => Bool -> Graph c -> [Graph c]
benchTravConv ex = fmap deserialise . traverse True ex

benchTravMergeConv :: (Keyless c) => Bool -> Graph c -> Graph c
benchTravMergeConv ex = deserialise . concat . traverse False ex

benchTravConvMerge :: (Keyless c) => Bool -> Graph c -> Graph c
benchTravConvMerge ex = mergeAllGraphs . fmap deserialise . traverse True ex

-- -----------------------------------------------------------------------------

-- | Taken from the benchmarks for unordered-containers
data B where
  B :: NFData a => a -> B

instance NFData B where
  rnf (B b) = rnf b

multiplicity :: Int
multiplicity = 3

repeatedGraph :: (Keyless c) => Graph c
repeatedGraph = toGraph $ multipleListGraph multiplicity

intermingledGraph :: (Keyless c) => Graph c
intermingledGraph = toGraph $ multipleIntermingledListGraph multiplicity

lazyMapKL :: Graph ML.KeylessMap
lazyMapKL = repeatedGraph

lazyMapKLI :: Graph ML.KeylessMap
lazyMapKLI = intermingledGraph

lazyIntMapKL :: Graph IL.KeylessIntMap
lazyIntMapKL = repeatedGraph

lazyIntMapKLI :: Graph IL.KeylessIntMap
lazyIntMapKLI = intermingledGraph

vectorKL :: Graph V.KeylessVector
vectorKL = repeatedGraph

vectorKLI :: Graph V.KeylessVector
vectorKLI = intermingledGraph
