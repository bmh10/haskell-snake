module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import System.Random
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 5
width = 420 -- 28 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileHoriz = 27
pacmanInitialPos = (1,1)
pacmanInitialLives = 3
pacmanInitialDir = East
ghostInitialDir = East
window = InWindow "Snake" (width, height) (offset, offset)
background = black

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data GameState = Playing | Won | Lost deriving (Eq, Show) 

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East
oppositeDir None  = None

randomDir :: StdGen -> (Direction, StdGen)
randomDir g = (toEnum $ r, g') where (r, g') = randomR (0,3) g

data PacmanGame = Game
  { 
    level :: [String],           -- Updated level layout
    initialLevel :: [String],    -- Initial level layout
    pacmanPos :: (Int, Int),     -- Tile coord of pacman
    pacmanDir :: Direction,      -- Pacman's direction of travel
    pacmanNextDir :: Direction,  -- Buffered next direction
    score :: Int,                -- Current score 
    lives :: Int,                -- Current lives
    seconds :: Float,            -- Game timer
    gen :: StdGen,               -- Random number generator
    scaredTimer :: Int,          -- Scared ghost timer
    paused :: Bool,              -- Paused or not
    countdownTimer :: Int,       -- Start of game timer
    gameState :: GameState      -- State of the game
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> PacmanGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PacmanGame -> PacmanGame
setTile x y c g = g {level = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

onTick :: PacmanGame -> Bool -> Int -> a -> a -> a 
onTick g c t a b = if (c && (mod (round (seconds g)) t) == 0) then a else b

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

-- Rendering
render :: PacmanGame -> Picture 
render g = pictures [renderLevel g, 
                     renderDashboard g,
                     renderMessage g]

renderDashboard :: PacmanGame -> Picture
renderDashboard g = pictures $ [scorePic, livesTxt] ++ livesPic
  where
    scorePic = color white $ translate (-80) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    livesTxt = color white $ translate 20 (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text "Lives:"
    livesPic = genLivesPic (lives g)

    genLivesPic :: Int -> [Picture]
    genLivesPic 0 = [blank]
    genLivesPic n = (translate (50 + fromIntegral n*tileSize) (-fromIntegral height/2 + 10) $ GG.png "img/pacmanEast2.png") : genLivesPic (n-1)

renderMessage :: PacmanGame -> Picture
renderMessage g = pictures [countdownPic, statusMsg]
  where 
    countdownPic = if (countdownTimer g) > 0 then color white $ translate 0 10 $ scale 0.3 0.3 $ text $ show $ countdownTimer g else blank
    statusMsg    = if (gameState g) /= Playing then color white $ translate (-100) 10 $ scale 0.3 0.3 $ text msg else blank
    msg          = if (gameState g) == Won then "You won" else "Game Over"

renderLevel :: PacmanGame -> Picture
renderLevel game = renderLines (level game) 0

renderLines :: [String] -> Int -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y+1)]

renderLine :: String -> Int -> Int -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+1) y]

renderTile :: Char -> Int -> Int -> Picture
renderTile c x y
 | c == 'x'  = translate x' y' $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == '+'  = translate x' y' $ color white $ rectangleSolid (tileSize-1) 2
 | c == '.'  = translate x' y' $ color yellow $ circleSolid 2
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> PacmanGame -> PacmanGame
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g  = setPacmanDir East g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g   = setPacmanDir West g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g     = setPacmanDir North g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g   = setPacmanDir South g
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game
 | (gameState game) /= Playing = resetGameFully game
 | otherwise = game

setPacmanDir :: Direction -> PacmanGame -> PacmanGame
setPacmanDir dir g
 | (pacmanDir g) == oppositeDir dir = g { pacmanDir = dir, pacmanNextDir = None } 
 | otherwise                        = g { pacmanNextDir = dir }

-- Have to update lives twice to prevent missed collision
update :: Float -> PacmanGame -> PacmanGame
update secs game
 | (paused game)               = game
 | (gameState game) /= Playing = game
 | (countdownTimer game) > 0   = onTick game True 4 (decrementCountdown $ updateSeconds game) (updateSeconds $ game)
 | otherwise                   = updateScore $ updatePacman $ updateSeconds game

updateSeconds :: PacmanGame -> PacmanGame
updateSeconds game = game {seconds = (seconds game) + 1, scaredTimer = (scaredTimer game) + 1}

decrementCountdown :: PacmanGame -> PacmanGame
decrementCountdown game = game {countdownTimer = (countdownTimer game) - 1}

updateScore :: PacmanGame -> PacmanGame
updateScore g
  | tile == '.'      = setBlankTile $ g { score = s + 10 }
  | tile == 'o'      = setBlankTile $ g { score = s + 50 }
  | otherwise        = g
  where
    (x, y) = pacmanPos g
    s = score g
    tile = getTile x y g
    setBlankTile = setTile x y '_'

updatePacman :: PacmanGame -> PacmanGame
updatePacman g = updatePacmanPos g

updatePacmanPos :: PacmanGame -> PacmanGame
updatePacmanPos g
 | canMove (x, y) nextDir g = g { pacmanPos = (move (x, y) nextDir), pacmanDir = nextDir, pacmanNextDir = None }
 | canMove (x, y) dir g     = g { pacmanPos = (move (x, y) dir) }
 | otherwise                = g
  where
    dir = pacmanDir g
    nextDir = pacmanNextDir g
    (x, y) = pacmanPos g

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) None = (x, y)
move (x, y) East = (wrapx $ x+1, y)
move (x, y) West = (wrapx $ x-1, y)
move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)

canMove ::  (Int, Int) -> Direction -> PacmanGame -> Bool
canMove _ None _ = False
canMove (x, y) dir g = canMoveTo g dir $ move (x, y) dir

canMoveTo :: PacmanGame -> Direction -> (Int, Int) -> Bool
canMoveTo g dir (x, y) = getTile x y g /= 'x' && not (getTile x y g == '+' && dir == South)

wrapx :: Int -> Int
wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

resetGame :: PacmanGame -> PacmanGame
resetGame g = g { pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, seconds = 0, pacmanNextDir = None, scaredTimer = 0, countdownTimer = 3}

resetGameFully :: PacmanGame -> PacmanGame
resetGameFully g = resetGame $ g {gameState = Playing, lives = pacmanInitialLives, score = 0, level = (initialLevel g)}

initTiles = do 
  contents <- readFile "snake.lvl"
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, initialLevel = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, score = 0, seconds = 0, lives = pacmanInitialLives, pacmanNextDir = None, gen = stdGen, scaredTimer = 0, paused = False, countdownTimer = 3, gameState = Playing }
  print rows
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
