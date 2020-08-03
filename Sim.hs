{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Sim where

import Prelude hiding (id, not, or, and, sum)
import qualified Prelude as P

import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as CMS
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
import qualified Data.Function as F
import Control.Monad
import Data.Foldable (toList)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Linear.V2
import Linear.V3

-- Utilities
data HList :: [*] -> * where
    HNil :: HList '[]
    (:>) :: a -> HList t -> HList (a ': t)

infixr :>

nextStr :: String -> String
nextStr [] = "a"
nextStr ('z':xs) = 'a':nextStr xs
nextStr (x:xs) = succ x:xs

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

vuncurry :: (a -> a -> b) -> (V2 a -> b)
vuncurry f = \(V2 x y) -> f x y

vuncurry3 :: (a -> a -> a -> b) -> (V3 a -> b)
vuncurry3 f = \(V3 x y z) -> f x y z

vfst, vsnd :: V2 a -> a
vfst (V2 x _) = x
vsnd (V2 _ y) = y

vzip :: [a] -> [a] -> [V2 a]
vzip = zipWith V2

vzip3 :: [a] -> [a] -> [a] -> [V3 a]
vzip3 = zipWith3 V3

tr, fa :: Bool
tr = True
fa = False

-- Datatypes/newtypes
newtype WireState = WireState { unWireState :: Bool }
    deriving (Ord, Eq)
instance Show WireState where
    show (WireState True) = "1"
    show (WireState False) = "0"

on, off :: WireState
on = WireState True
off = WireState False
bmap :: (Bool -> Bool) -> WireState -> WireState
bmap f = WireState . f . unWireState
bmap2 :: (Bool -> Bool -> Bool) -> WireState -> WireState -> WireState
bmap2 f a b = WireState $ F.on f unWireState a b

newtype WireId = WireId String
    deriving (Ord, Eq, Monoid, Semigroup, IsString)

instance Enum WireId where
    succ (WireId orig) = WireId $ nextStr orig
    pred = error "pred @WireId : Can only succ WireId, not pred them" 
    toEnum = error "toEnum @WireId : Can only succ WireId" 
    fromEnum = error "fromEnum @WireId : Can only succ WireId" 

instance Bounded WireId where
    minBound = WireId "a"
    maxBound = error "No minimum bound for WireId"

instance Show WireId where
    show (WireId id) = concat ["<", id, ">"]

newtype ComponentId = ComponentId String
    deriving (Ord, Eq, Monoid, Semigroup, IsString)
instance Show ComponentId where
    show (ComponentId id) = concat ["{", id, "}"]

instance Enum ComponentId where
    succ (ComponentId orig) = ComponentId $ nextStr orig
    pred = error "pred @ComponentId : Can only succ ComponentId, not pred them" 
    toEnum = error "toEnum @ComponentId : Can only succ ComponentId" 
    fromEnum = error "fromEnum @ComponentId : Can only succ ComponentId" 

instance Bounded ComponentId where
    minBound = ComponentId "a"
    maxBound = error "No minimum bound for ComponentId"


data Component = Nand { a :: WireId, b :: WireId, out :: WireId }
               -- | Glue { a :: WireId, b :: WireId }
               | High { out :: WireId }
    deriving (Show, Eq, Ord)

compWires :: Component -> [WireId]
compWires Nand {..} = [a,b,out]
-- compWires Glue {..} = [a,b]
compWires High {..} = [out]

-- Pairings of datatypes / newtypes into circuits
type CircuitState = M.Map WireId (S.Set ComponentId)
type CircuitWiring = M.Map ComponentId Component
type Circuit = (CircuitState, CircuitWiring)

-- Print a circuit's state, in terms of active wires
pretty :: Circuit -> IO ()
pretty (state, wiring) = do
    let allWires = concatMap compWires $ M.elems wiring
    let allState = foldr (M.alter (\x -> Just $ fromMaybe S.empty x)) state allWires
    let showWire wire components
            = putStrLn $ unwords
            $ [show wire, "|", show $ wireFromSet components, "|"] ++ map show (S.toList components)
    mapM_ (uncurry showWire) $ M.toList allState

-- Step circuits forward
wireFromState :: CircuitState -> WireId -> WireState
wireFromState state id = maybe off wireFromSet $ M.lookup id state
wireFromSet :: S.Set ComponentId -> WireState
wireFromSet = WireState . P.not . S.null

updateWire :: CircuitState -> WireId -> ComponentId -> WireState -> CircuitState
updateWire circuit id source state = M.alter (Just . f . fromMaybe S.empty) id circuit
    where
        f = if state == on
               then S.insert source
               else S.delete source

getOut :: CircuitState -> Component -> [(WireId, WireState)]
getOut initial Nand {..} = [(out, bmap P.not $ bmap2 (&&) (initial `wireFromState` a) (initial `wireFromState` b))]
-- getOut initial Glue {..} = let collective = bmap2 (||) (initial `wireFromState` a) (initial `wireFromState` b)
--                             in [(a, collective), (b, collective)]
getOut _       High {..} = [(out, on)]

apply :: CircuitState -> (ComponentId, Component) -> Endo CircuitState
apply initial (id, component) = foldMap updateFinal (getOut initial component)
    where
    updateFinal (out, state) = Endo $ \final -> updateWire final out id state

singleStep :: Circuit -> Circuit
singleStep (initial, wiring) = (appEndo (foldMap (apply initial) (M.toList wiring)) initial, wiring)

step :: Circuit -> Circuit
step circuit@(state, wiring) =
    let (nextState, _) = singleStep circuit
     in if nextState /= state
        then step (nextState, wiring)
        else (nextState, wiring)

stepClean :: CircuitWiring -> Circuit
stepClean = step . cleanState

cleanState :: CircuitWiring -> Circuit
cleanState = (M.empty,)

-- Construct circuits, simply
data CircuitTree = Namespaces (M.Map ComponentId CircuitTree) | Component Component

group :: [(ComponentId, CircuitTree)] -> CircuitTree
group = Namespaces . M.fromList

makeBaseFunctor ''CircuitTree

flatten :: CircuitTree -> CircuitWiring
flatten tree = M.fromList $ cata helper tree
    where
    helper (ComponentF component) = [(mempty, component)]
    helper (NamespacesF namespaces) = foldMap f $ M.toList namespaces
        where
        f (id, subtrees) = mapFst (("." <> id) <>) <$> subtrees

-- A monad for building circuits
newtype CircuitM a = CircuitM { unCircuitM :: CMS.State (WireId, ComponentId) a }
    deriving (Functor, Applicative, Monad)

deriving instance CMS.MonadState (WireId, ComponentId) CircuitM

newWire :: WireId -> CircuitM WireId
newWire prefix = do
    newId <- CMS.gets fst
    CMS.modify $ mapFst succ
    pure $ prefix <> "_" <> newId

newComponent :: ComponentId -> CircuitM ComponentId
newComponent prefix = do
    newId <- CMS.gets snd
    CMS.modify $ mapSnd succ
    pure $ prefix <> "_" <> newId

unsafeNewWire :: WireId -> CircuitM WireId
unsafeNewWire = pure

unsafeNewComponent :: ComponentId -> CircuitM ComponentId
unsafeNewComponent = pure

groupM :: [(ComponentId, CircuitM CircuitTree)] -> CircuitM CircuitTree
groupM = fmap (group . getCompose) . sequence . Compose

runCircuitM :: CircuitM CircuitTree -> CircuitWiring
runCircuitM m = flatten $ CMS.evalState (unCircuitM m) ("a", "a")

data BoolSpec = BoolSpec { isSafe :: Bool, value :: Bool, name :: String }
safe, unsafe :: Bool -> String -> BoolSpec
safe = BoolSpec True
unsafe = BoolSpec False

boolSpecToWC :: BoolSpec -> CircuitM (WireId, (ComponentId, CircuitTree))
boolSpecToWC BoolSpec {..} = do
    wire <- (if isSafe then newWire else unsafeNewWire) $ fromString name
    componentId <- (if isSafe then newComponent else unsafeNewComponent) $ fromString name
    component <- boolM value wire
    pure (wire, (componentId, component))

boolify :: Boolify a b => a -> HList b -> CircuitM CircuitTree
boolify f = fmap group . boolifyHelper f

class Boolify a (b :: [*]) | a -> b where
    boolifyHelper :: a -> HList b -> CircuitM [(ComponentId, CircuitTree)]

instance Boolify (CircuitM CircuitTree) '[] where
    boolifyHelper circuit _ = fmap ((:[]) . ("root",)) circuit

instance Boolify a b => Boolify (WireId -> a) (BoolSpec ': b) where
    boolifyHelper f (spec :> rest) = boolifyHelper (f . runIdentity) (Identity spec :> rest)

instance (Traversable t, Foldable t, Functor t, Boolify a b) => Boolify (t WireId -> a) (t BoolSpec ': b) where
    boolifyHelper f (subspecs :> rest) = do
        wccs <- traverse boolSpecToWC subspecs
        let wires = fst <$> wccs
        let components = toList $ snd <$> wccs
        restComponents <- boolifyHelper (f wires) rest
        pure $ components ++ restComponents

----------------------------- BASIC CIRCUITS -----------------------------------
onM, offM :: WireId -> CircuitM CircuitTree
onM out = pure $ Component $ High out
offM _ = groupM []

boolM :: Bool -> WireId -> CircuitM CircuitTree
boolM bool out = if bool then onM out else offM out

nand :: WireId -> WireId -> WireId -> CircuitM CircuitTree
nand a b out = pure $ Component $ Nand {..}

--glue :: WireId -> WireId -> CircuitM CircuitTree
--glue a b = pure $ Component $ Glue {..}

not :: WireId -> WireId -> CircuitM CircuitTree
not inp out = let a = inp; b = inp in pure $ Component $ Nand {..}

to :: WireId -> WireId -> CircuitM CircuitTree
to inp out = do
    internal <- newWire "internal"
    groupM
        [("n1", not inp internal)
        ,("n2", not internal out)
        ]

and :: WireId -> WireId -> WireId -> CircuitM CircuitTree
and a b out = do
    internal <- newWire "internal"
    groupM
        [("nand", nand a b internal)
        ,("not", not internal out)
        ]

andN :: [WireId] -> WireId -> CircuitM CircuitTree
andN []    _   = error "Must have at least one input to andN"
andN [inp] out = groupM [("single", to inp out)]
andN inps  out = do
    let halve xs = splitAt (length xs `div` 2) xs
    let (inpsR, inpsL) = halve inps
    wireL <- newWire "and_left"
    wireR <- newWire "and_right"
    groupM
        [("left", andN inpsR wireR)
        ,("right", andN inpsL wireL)
        ,("together", and wireL wireR out)
        ]

or :: WireId -> WireId -> WireId -> CircuitM CircuitTree
or a b out = do
    na <- newWire "na"
    nb <- newWire "nb"
    groupM
        [("not_a", not a na)
        ,("not_b", not b nb)
        ,("nand", nand na nb out)
        ]

xor :: WireId -> WireId -> WireId -> CircuitM CircuitTree
xor a b out = do
    na <- newWire "na"
    nb <- newWire "nb"
    tf <- newWire "tf"
    ft <- newWire "ft"
    groupM
        [("not_a", not a na)
        ,("not_b", not b nb)
        ,("tf", and a nb tf)
        ,("ft", and na b ft)
        ,("out", or tf ft out)
        ]

memory :: WireId -> WireId -> WireId -> CircuitM CircuitTree
memory set inp out = do
    a <- newWire "a"
    b <- newWire "b"
    c <- newWire "c"
    groupM
        [("c1", nand inp set a)
        ,("c2", nand a set b)
        ,("c3", nand a c out)
        ,("c4", nand b out c)
        ]

memoryN :: WireId -> Compose [] V2 WireId -> CircuitM CircuitTree
memoryN set (getCompose -> io) = do
    let components = map (vuncurry $ memory set) io
    groupM $ zip [minBound..] components

enabler :: WireId -> Compose [] V2 WireId -> CircuitM CircuitTree
enabler enable (getCompose -> io) = do
    let components = map (vuncurry $ and enable) io
    groupM $ zip [minBound..] components

register :: WireId -> WireId -> Compose [] V2 WireId -> CircuitM CircuitTree
register set enable (getCompose -> io) = do
    internal <- replicateM (length io) (newWire "")
    let inpToInternal = vzip (vfst <$> io) internal
    let internalToOut = vzip internal (vsnd <$> io)
    groupM
        [("memory",  memoryN set    $ Compose inpToInternal)
        ,("enabler", enabler enable $ Compose internalToOut)
        ]

busRegister :: WireId -> WireId -> [WireId] -> CircuitM CircuitTree
busRegister set enable bus = register set enable (Compose $ vzip bus bus)

halfAdder :: WireId -> WireId -> WireId -> WireId -> CircuitM CircuitTree
halfAdder a b sum carry =
    groupM
        [("summer", xor a b sum)
        ,("carrier", and a b carry)
        ]

adder :: WireId -> WireId -> WireId -> WireId -> WireId -> CircuitM CircuitTree
adder a b carryIn sum carryOut = do
    abSum <- newWire "abSum"
    abCarry <- newWire "abCarry"
    sumInCarry <- newWire "sumInCarry"
    groupM
        [("halfAdderAB", halfAdder a b abSum abCarry)
        ,("halfAdderSumIn", halfAdder abSum carryIn sum sumInCarry)
        ,("or", or abCarry sumInCarry carryOut)
        ]

adderN :: WireId -> Compose [] V3 WireId -> WireId -> CircuitM CircuitTree
adderN carryIn (getCompose -> abOutWires) carryOut = do
    circuits <- helper carryIn abOutWires carryOut
    namedCircuits <- zipWithM nameCircuits ([0..] :: [Int]) circuits
    pure $ group namedCircuits
    where
    nameCircuits ii circuit = do
        componentId <- newComponent $ "adder" <> fromString (show ii)
        pure (componentId, circuit)
    helper :: WireId -> [V3 WireId] -> WireId -> CircuitM [CircuitTree]
    helper _   []                _    = error "Cannot have an adder with no wires"
    helper cin [V3 a b sum]      cout = sequence [adder a b cin sum cout]
    helper cin ((V3 a b sum):xs) cout = do
        internal <- newWire "internal"
        (:) <$> adder a b internal sum cout <*> helper cin xs internal

ram1 :: V2 WireId -> WireId -> WireId -> [WireId] -> CircuitM Component
ram1 = undefined

-- Assumption: second list has 2 ^ n wires of the first one
decoder :: [WireId] -> [WireId] -> CircuitM CircuitTree
decoder inps outs = do
    let iis = fromString . show <$> ([0..] :: [Integer])
    ninps <- replicateM (length inps) $ newWire "ninp"
    notComponents <- forM (zip3 iis inps ninps)
                   $ \(id, inp, out) -> do
                       comp <- not inp out
                       name <- newComponent id
                       pure (name, comp)
    let possibilities = sequence $ replicate (length inps) [False, True]
    anders <- forM (zip3 iis possibilities outs)
            $ \(id, bits, out) -> do
                let wires = zipWith3 (\b inp ninp -> if b then inp else ninp) bits inps ninps
                comp <- andN wires out
                name <- newComponent id
                pure (name, comp)
    pure $ group $ notComponents ++ anders

