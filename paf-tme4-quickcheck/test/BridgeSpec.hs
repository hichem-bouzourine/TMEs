module BridgeSpec where

import Test.Hspec
import Test.QuickCheck

import Bridge

property_inv_initBridge :: Int -> Property
property_inv_initBridge lim =
  (prop_pre_initBridge lim) ==> property $ prop_inv_IslandBridge (initBridge lim)

-- Pour tester directement dans ghci :
-- stack ghci --test --main-is Spec
-- (choisir   Spec comme module main)
-- puis dans ghci :
-- blabla...> :set prompt "> "
-- > import Test.QuickCheck
-- > quickCheck property_inv__initBridge

-- (et maintenir ghci ouvert pour la suite, et :reload
-- en cas de changement de code source)

bridgeSpecInit = do
  describe "initBridge" $ do
    it "preserves the invariant" $ property $ \lim -> property_inv_initBridge lim

-- Exemple de générateur aléatoire monadique (cf. cours 8)
genBridgeFree :: Gen IslandBridge
genBridgeFree = do
    lim <- choose (1, 100)  -- la limite initiale
    nbI <- choose (0, 50)
    nbTo <- choose (0, 50)
    nbFrom <- choose (0, 50)
    return $ if nbI + nbTo + nbFrom == lim
             then BridgeClosed lim nbTo nbFrom
             else BridgeOpened lim nbTo nbI nbFrom

-- Exemple de générateur garantissant l'invariant
genBridgeOk :: Gen IslandBridge
genBridgeOk = do
    lim <- choose (1, 100)  -- la limite initiale
    nbCars <- choose (0, lim)  -- 
    nbI <- choose (0, nbCars)
    nbTo <- choose (0, nbI)
    let nbFrom = nbCars - (nbI + nbTo)
    return $ mkBridge lim nbTo nbI nbFrom

property_inv_genBridgeOK :: Property
property_inv_genBridgeOK = forAll genBridgeOk $ prop_inv_IslandBridge

-- quickCheck prop_genBridgeOK_inv

bridgeSpecGenOk = do
  describe "genBridgeOk" $ do
    it "generates bridges that satisfy their invariant" $
      property property_inv_genBridgeOK

property_inv_genBridgeFree :: Property
property_inv_genBridgeFree = forAll genBridgeFree $ prop_inv_IslandBridge
-- quickCheck prop_genBridgeFree_inv

bridgeSpecGenFree = do
  describe "genBridgeFree" $ do
    it "generates bridges that satisfy their invariant" $
      property property_inv_genBridgeFree

-- Générateur par défaut, qui peut être incohérent dans max. 20% des cas
-- (ce qui permet de tester les préconditions)
instance Arbitrary IslandBridge where
  arbitrary =
    frequency [(2, genBridgeFree) -- 20% de génération libre
              , (8, genBridgeOk)] -- 80% de génération sûre

-- quickCheck islandBridge_inv

property_inv_enterToIsland :: IslandBridge -> Property
property_inv_enterToIsland b =
  (prop_inv_IslandBridge b)
  && (prop_pre_enterToIsland b)
  ==> classify (bridgeLimit b < 10) "small capacity (<10)" $
  classify (bridgeLimit b < 50) "medium capacity (<50)" $
  classify (bridgeLimit b <= 100) "large capacity (>=50)" $
  property $ prop_inv_IslandBridge (enterToIsland b)

-- >>> quickCheck prop_enterToIsland_inv

enterToIslandSpec = do
  describe "enterToIsland" $ do
    it "preserves the invariant" $
      property property_inv_enterToIsland
