{-# LANGUAGE TemplateHaskell, LambdaCase  #-}

module Lib
  ( Tape(..)
  )
where

import           Control.Monad.State
import qualified Data.Vector                   as V
import           Control.Lens
import qualified Data.Vector.Generic           as VG
import           Data.Vector.Generic.Mutable    ( write )
import           Data.Char                      ( chr
                                                , ord
                                                )
import           Data.Maybe                     ( mapMaybe )

data Tape = Tape { _memory :: V.Vector Int, _cursor :: Int }
  deriving (Show)
makeLenses ''Tape

type BFState a = StateT Tape IO a
data Direction = Left' | Right'

data BFCommand
  = BFInc
  | BFDec
  | BFMRight
  | BFMLeft
  | BFInput
  | BFOutput
  | BFLoopL
  | BFLoopR deriving (Eq)

type BFProgram = [BFCommand]

char2Command :: Char -> Maybe BFCommand
char2Command '+' = Just BFInc
char2Command '-' = Just BFDec
char2Command '>' = Just BFMRight
char2Command '<' = Just BFMLeft
char2Command ',' = Just BFInput
char2Command '.' = Just BFOutput
char2Command '[' = Just BFLoopL
char2Command ']' = Just BFLoopR
char2Command _   = Nothing

execCommand :: BFCommand -> BFState ()
execCommand BFInc    = increment
execCommand BFDec    = decrement
execCommand BFMLeft  = moveCursor Left'
execCommand BFMRight = moveCursor Right'
execCommand BFOutput = outputPtr
execCommand BFInput  = getInput


execLoop :: BFProgram -> BFState ()
execLoop instrs = getCell >>= \case
  0 -> pure ()
  _ -> execCommandList instrs *> execLoop instrs

execCommandList :: BFProgram -> BFState ()
execCommandList [] = pure ()
execCommandList (BFLoopL : xs) =
  let (loop, (_ : rest)) = span (/= BFLoopR) xs
  in  execLoop loop *> execCommandList rest
execCommandList (x : xs) = execCommand x *> execCommandList xs

parse :: String -> BFProgram
parse = mapMaybe char2Command

emptyTape :: Tape
emptyTape = Tape (VG.replicate 256 0) 0

moveCursor :: Direction -> BFState ()
moveCursor Right' = cursor += 1
moveCursor Left'  = cursor -= 1


getCell :: BFState Int
getCell = do
  ptr <- gets (view cursor)
  mem <- gets (view memory)
  pure $ mem ^?! ix ptr

increment :: BFState ()
increment = getCell >>= modifyCell . succ

decrement :: BFState ()
decrement = getCell >>= modifyCell . pred


modifyCell :: Int -> BFState ()
modifyCell n = do
  ptr <- gets (view cursor)
  memory %= writeCell ptr
 where
  writeCell :: Int -> V.Vector Int -> V.Vector Int
  writeCell ptr vector = VG.modify (\v -> write v ptr n) vector

outputPtr :: BFState ()
outputPtr = getCell >>= lift . putChar . chr

getInput :: BFState ()
getInput = lift getChar >>= modifyCell . ord

run :: String -> IO ()
run s = evalStateT (execCommandList $ parse s) emptyTape
