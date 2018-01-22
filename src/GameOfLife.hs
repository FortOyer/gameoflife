{-|
Module      : GameOfLife
Description : Game of Life Reactive Banana Example
Copyright   : (c) Archibald Neil MacDonald, 2018
License     : BSD3
Maintainer  : FortOyer@hotmail.co.uk
Stability   : experimental
Portability : POSIX

Example Game of Life program using reactive-banana-wx and reactive-banana.
-}
{-# LANGUAGE RecursiveDo #-}

module GameOfLife
    ( runGame
    ) where

import Control.Monad

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

import Data.Array

-- | Defines the width & height of the conway game board.
width, height :: Int
width = 20
height = 20

-- | Alias for coordinate system we use for the board.
--   A square is an (x, y) coordinate.
type Square = (Int, Int)

-- | Defines the (min, max) coordinates of the board.
boardBounds :: (Square, Square)
boardBounds = ((1, 1), (width, height))

-- | Sets up the game window and FRP network connecting up everything.
runGame :: IO ()
runGame = start $ do
  window <- frame [ text := "Conway's Game of Life"
                  , bgcolor := white ]

  -- Create a timer that triggers to update board every 100 ms.
  t  <- timer window [ interval := 100 ]

  -- Pause Button
  pauseButton <- button window [size := sz 40 40]
  
  -- Create our 2D Checkbox list based on width*height. 
  checks <- replicateM height $
    replicateM width $ checkBox window [text := "  "]
  
  -- Place our checkboxes in our window.
  set window [layout := column 5 [ grid 1 1 $ (fmap . fmap) widget checks
                                 , widget pauseButton]]
  
  -- Convert our checkboxes into an indexable array to match the board.
  let checkLookup = listArray boardBounds (concat checks)

  let networkDescription :: MomentIO ()
      networkDescription = mdo
        
        -- Grab pause event button presses
        ePause <- event0 pauseButton command
        -- Turn into a time-dependent alternating bool when pressed.
        pauseSwitch <- accumB False (not <$ ePause)
        -- Timer enabled is dictated by event trigger.
        sink t [ enabled :== pauseSwitch]
        -- As is text of pause button itself.
        sink pauseButton [ text :== fmap (\b ->
          if b then "Pause" else "Resume") pauseSwitch]

        etick  <- event0 t command -- Convert WX time event into FRP event.
        ev <- events checkLookup   -- Collect our trigger events.
        -- Create a brand new stream based on initial configuration. 
        -- Is a combination of time and checkbox click events.
        eState <- accumB newBoard $ unions [ev, nextIteration <$ etick]

        -- Update the checkboxes on/off state based upon current board
        -- configuration.
        mapM_ (\(i, w) ->
          sink w [checked :== fmap (\b -> alive $ b ! i) eState]) $
            assocs checkLookup 

  network <- compile networkDescription -- Generate network
  actuate network -- run network.

-- | Using a given checkbox's trigger event, transforms it into a new event 
--   containing a function where the square at coordinate (x,y) which matches
--   the checkbox (x, y) is now forced Alive.
--
--   [@(i, w)@]: Tuple of coordinate position and checkbox.
activateCellEvent :: (Square, CheckBox ()) -> MomentIO (Event (Board -> Board))
activateCellEvent (i, w) = do
  e <- event0 w command 
  return $ (\b -> b // [(i, Alive)]) <$ e

-- | For every checkbox, we create a new list of events using
--  'GameOfLife.activateCellEvent' and merge these all down into one event.
events :: Array Square (CheckBox ()) -> MomentIO (Event (Board -> Board))
events c = do
  events <- sequence $ fmap activateCellEvent (assocs c)
  return $ unions events 

-- Game logic -----------------------------------------------------------------

-- | Game of Life Cell. A cell's configuration is binary.
--   Can only be alive or dead.
data Cell = Dead | Alive deriving (Eq)

-- | Helper function. Returns true if cell is alive. Otherwise returns false.
alive :: Cell -> Bool
alive Alive = True
alive Dead  = False

-- | Defines the board type. A Board is just a 2D array of cells.
type Board = Array Square Cell

-- | Creates the initial (blank) board.
newBoard  :: Board
newBoard = listArray boardBounds (repeat Dead)

-- | Takes a board and generates the next configuration of the board.
--
--   [@b@]: Current board configuration
nextIteration :: Board -> Board
nextIteration b  = array boardBounds $ zip idxs newCells 
  where idxs     = indices b
        newCells = fmap (`cellStatus` b) idxs

-- | Returns whether a given coordinate is within the bounds of the board's
--   minimum and -- maximum coordinates.
--
--   [@(x, y)@]: Coordinate of square on board.
inBounds :: Square -> Bool
inBounds (x, y) 
  | x < 1 = False
  | x > width  = False
  | y < 1 = False
  | y > height = False
  | otherwise  = True

-- | Returns the number of neighbours that are alive around a cell. From a
--   minimum of 0 or maximum of 8 (number of neighbours horizontal, vertical,
--   and diagonal.)
--
--   [@(x, y)@]: Coordinate of square on board.
--
--   [@b@]: Current board configuration
countNeighbours :: Square -> Board -> Int
countNeighbours (x, y) b =
  foldr (\z a -> if alive (b ! z) then a + 1 else a) 0 $ filter inBounds
     [(x - 1, y),     (x + 1, y),     (x - 1, y - 1), (x + 1, y - 1), 
      (x - 1, y + 1), (x + 1, y + 1), (x, y - 1),     (x, y + 1)]

-- | Return the "next" state of a given cell based on the current board
--   configuration.
--
--   Conway follows these rules:
--
--   1. If a cell has less than 2 neighbours it's now dead. (Lonely)
--
--   2. If a cell has more than 3 neighbours it's now dead. (Overcrowded.)
--
--   3. If a cell is dead and has exactly 3 neighbours it's now alive.
--
--   4. Otherwise the cell does not change.
--
--   [@q@]: (x, y) coordinate of square on board.
--
--   [@b@]: Current board configuration
cellStatus :: Square -> Board-> Cell
cellStatus q b
  | cn < 2          = Dead
  | cn > 3          = Dead
  | dead && cn == 3 = Alive
  | otherwise       = status
  where status = b ! q
        dead   = not $ alive status
        cn     = countNeighbours q b
