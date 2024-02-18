module Bridge where

data IslandBridge =
  BridgeOpened Int Int Int Int 
  | BridgeClosed Int Int Int
  deriving Show

-- smart constructor
mkBridge :: Int -> Int -> Int -> Int -> IslandBridge
mkBridge lim nbTo nbI nbFrom
  | nbTo + nbI + nbFrom < lim = BridgeOpened lim nbTo nbI nbFrom
  | nbTo + nbI + nbFrom == lim = BridgeClosed lim nbTo nbFrom
  | otherwise = error "Wrong bridge (limit exceeded)"

-- accessors
nbCarsOnIsland :: IslandBridge -> Int
nbCarsOnIsland (BridgeOpened _ _ nbI _) = nbI
nbCarsOnIsland (BridgeClosed lim nbTo nbFrom) = lim - (nbFrom + nbTo)

nbCarsToIsland :: IslandBridge -> Int
nbCarsToIsland (BridgeOpened _ nbTo _ _) = nbTo
nbCarsToIsland (BridgeClosed _ nbTo _) = nbTo

nbCarsFromIsland :: IslandBridge -> Int
nbCarsFromIsland (BridgeOpened _ _ _ nbFrom) = nbFrom
nbCarsFromIsland (BridgeClosed _ _ nbFrom) = nbFrom

nbCars :: IslandBridge -> Int
nbCars (BridgeOpened _ nbTo nbI nbFrom) = nbTo + nbI + nbFrom
nbCars (BridgeClosed lim _ _) = lim

bridgeLimit :: IslandBridge -> Int
bridgeLimit (BridgeOpened lim _ _ _) = lim
bridgeLimit (BridgeClosed lim _ _) = lim

-- invariant
prop_inv_IslandBridge :: IslandBridge -> Bool
prop_inv_IslandBridge b@(BridgeOpened lim _ _ _) =
  0 <= nbCars b && nbCars b <= lim
prop_inv_IslandBridge b@(BridgeClosed lim _ _) =
  nbCars b == lim

-- initialisation
initBridge :: Int -> IslandBridge
initBridge lim = mkBridge lim 0 0 0

prop_pre_initBridge :: Int -> Bool
prop_pre_initBridge lim = lim > 0

-- opération : entrée du continent vers l'île
enterToIsland :: IslandBridge -> IslandBridge
enterToIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim (nbTo+1) nbI nbFrom
enterToIsland (BridgeClosed _ _ _) = error "Cannot enter bridge to island"

prop_pre_enterToIsland :: IslandBridge -> Bool
prop_pre_enterToIsland b@(BridgeOpened lim _ _ _) = nbCars b < lim
prop_pre_enterToIsland (BridgeClosed _ _ _) = False

leaveToIsland :: IslandBridge -> IslandBridge
leaveToIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim (nbTo-1) (nbI+1) nbFrom 
leaveToIsland (BridgeClosed _ _ _) = error "cannot leave bridge to Island - Bridge is closed" 

prop_pre_leaveToIsland :: IslandBridge -> Bool
prop_pre_leaveToIsland b@(BridgeOpened lim nbTo nbI nbFrom) = bridgeLimit b < lim && nbCarsToIsland b > 0 && nbCars b < lim && nbCars b > 0
prop_pre_leaveToIsland (BridgeClosed _ _ _) = False

enterFromIsland :: IslandBridge -> IslandBridge
enterFromIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim nbTo (nbI-1) $ nbFrom+1
enterFromIsland (BridgeClosed _ _ _) = error "cannot enter bridge from island"

prop_pre_enterFromIsland :: IslandBridge -> Bool
prop_pre_enterFromIsland b@(BridgeOpened lim _ _ _) = nbCarsOnIsland b > 0 && bridgeLimit b < lim
prop_pre_enterFromIsland (BridgeClosed _ _ _) = False

leaveFromIsland :: IslandBridge -> IslandBridge
leaveFromIsland (BridgeOpened lim nbTo nbI nbFrom) = mkBridge lim nbTo nbI $ nbFrom-1
leaveFromIsland (BridgeClosed _ _ _) = error "Cannot leave bridge"

prop_pre_leaveFromIsland :: IslandBridge -> Bool
prop_pre_leaveFromIsland b@(BridgeOpened lim _ _ _) = nbCarsFromIsland b > 0 && bridgeLimit b < lim
prop_pre_leaveFromIsland (BridgeClosed _ _ _) = False
