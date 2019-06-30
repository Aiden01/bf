{-# LANGUAGE TemplateHaskell, LambdaCase  #-}

module Lib
  ( run
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
execLoop instrs = use cell >>= \case
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

cell :: Lens' Tape Int
cell = lens getter setter
 where
  getter s =
    let ptr = s ^. cursor
        mem = s ^. memory
    in  mem ^?! ix ptr
  setter s n =
    let ptr = s ^. cursor in s & memory %~ (VG.modify $ \v -> write v ptr n)

emptyTape :: Tape
emptyTape = Tape (VG.replicate 256 0) 0

moveCursor :: Direction -> BFState ()
moveCursor Right' = cursor += 1
moveCursor Left'  = cursor -= 1




increment :: BFState ()
increment = cell += 1

decrement :: BFState ()
decrement = cell -= 1


outputPtr :: BFState ()
outputPtr = use cell >>= lift . putChar . chr

getInput :: BFState ()
getInput = do
  v <- ord <$> lift getChar
  modify (cell .~ v)

run :: String -> IO ()
run s = evalStateT (execCommandList $ parse s) emptyTape
