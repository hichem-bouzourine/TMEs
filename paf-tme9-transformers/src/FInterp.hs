{-# LANGUAGE BangPatterns #-}

-- | L'interprète Forth

module FInterp where

import System.IO (hFlush, stdout)

import Data.Functor.Identity (Identity (..))
-- import Control.Monad.Trans.Identity
import Control.Monad
import Control.Monad (mplus)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict -- Forth est strict

import FState

-- | La transformer stack que l'on utilise

type Forth = ExceptT FError (StateT FMachine IO) FValue
-- isomorphe à :  IO (State FMachine (Either FError FValue))

-- | pour injecter une validation dans la monade
hoistEither :: Applicative m => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

-- | pour injecter directement une erreur
hoistError :: Applicative m => e -> ExceptT e m a
hoistError err = hoistEither $ Left $ err

-- | pour injecter des opérations pures sur l'état
hoistPure :: (FMachine -> (ForthResult, FMachine)) -> Forth
hoistPure op = ExceptT $ StateT $ \fm -> return (op fm)

-- | pour débugger : affichage facultatif de la machine
showMachine :: Bool -> Forth
showMachine dbg = ExceptT $ StateT $ \fm -> do
  when dbg $ liftIO $ putStrLn (show fm)
  return (Right FNone, fm)

-- | interprétation des primitives
interpPrim :: FPrimitive -> Forth
interpPrim POP = hoistPure $ fpop
interpPrim DUP = hoistPure $ fdup
interpPrim EMIT = do
  x <- hoistPure fpop
  liftIO $ putStr (show x)
  return FNone

interpOper :: Foperation -> Forth
interpOper PLUS = hoistPure fplus
interpOper MINUS = hoistPure fminus
-- interpOper MINUS = -
-- interpOper TIMES = *
-- interpOper DEVISE = /
-- interOper SQUARE = square

-- | Interprétation de l'instruction IF
interpIf :: Forth
interpIf = do
  cond <- hoistPure fpop
  case cond of
    FBool True -> return FNone
    FBool False -> return FNone -- on saute le ELSE
    _ -> hoistError $ FErrNotImplemented "Non-boolean condition in IF"

-- | Interprétation de l'instruction ELSE
interpElse :: Forth
interpElse = return FNone

-- | Interprétation de l'instruction THEN
interpThen :: Forth
interpThen = return FNone


-- | interpretation des instructions
interpInstr :: FInstr -> Forth
interpInstr (FVal x) = hoistPure $ fpush x
interpInstr (FPrim prim) = interpPrim prim
interpInstr (FOp oper) = interpOper oper
interpInstr (FWord w) = hoistPure $ fword w
interpInstr (FDef w prog) = hoistPure $ fdef w prog
interpInstr (FWord "IF") = interpIf
interpInstr (FWord "ELSE") = interpElse
interpInstr (FWord "THEN") = interpThen


-- | gestion du compteur de programme (prochain instruction)
interpNext :: Bool -> Forth
interpNext dbg = do
  (FNext instr) <- hoistPure fnext
  showMachine dbg
  when dbg $ liftIO $ putStrLn $ "==> " <> (show instr)
  interpInstr instr

-- | Coeur de l'interprète (argument True = mode debug)
interpreter :: Bool -> Forth
interpreter dbg = ExceptT $ do
  x <- runExceptT $ interpNext dbg
  case x of
    Right v -> runExceptT $ interpreter dbg
    Left FErrEmptyProgram -> do
      liftIO $ putStrLn $ " Ok."
      return x
    Left err -> do
      liftIO $ putStrLn $ " Ko. " <> (show err)
      return x

-- | Exécution du programme spécifié
execProg :: Bool -> FProgram -> Forth
execProg dbg prog = do
  hoistPure $ fload prog
  x <- interpreter dbg
  return x
