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
snakeInitialTiles = [(5,5), (4,5)]
snakeInitialLives = 3
snakeInitialDir = East
foodInitialPos = (3,3)
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

--TODO
randomPos (x,y) = (x+5,y+5)

data SnakeGame = Game
  { 
    level :: [String],           -- Updated level layout
    initialLevel :: [String],    -- Initial level layout
    snakeDir :: Direction,       -- Snake's direction of travel
    snakeTiles :: [(Int, Int)],
    foodPos :: (Int, Int),
    score :: Int,                -- Current score 
    lives :: Int,                -- Current lives
    seconds :: Float,            -- Game timer
    gen :: StdGen,               -- Random number generator
    scaredTimer :: Int,          -- Scared ghost timer
    paused :: Bool,              -- Paused or not
    countdownTimer :: Int,       -- Start of game timer
    gameState :: GameState       -- State of the game
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> SnakeGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> SnakeGame -> SnakeGame
setTile x y c g = g {level = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

onTick :: SnakeGame -> Bool -> Int -> a -> a -> a 
onTick g c t a b = if (c && (mod (round (seconds g)) t) == 0) then a else b

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

-- Rendering
render :: SnakeGame -> Picture 
render g = pictures [renderSnake g,
                     renderFood g,
                     renderLevel g, 
                     renderDashboard g,
                     renderMessage g]

renderSnake :: SnakeGame -> Picture
renderSnake g = pictures $ renderSnakeTiles (snakeTiles g)

renderFood :: SnakeGame -> Picture
renderFood g = renderTile 'f' x y where (x,y) = (foodPos g)

renderSnakeTiles :: [(Int, Int)] -> [Picture]
renderSnakeTiles [] = []
renderSnakeTiles ((x,y):xs) = renderTile 's' x y : renderSnakeTiles xs
 
renderDashboard :: SnakeGame -> Picture
renderDashboard g = pictures $ [scorePic, livesTxt] ++ livesPic
  where
    scorePic = color white $ translate (-80) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)
    livesTxt = color white $ translate 20 (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text "Lives:"
    livesPic = genLivesPic (lives g)

    genLivesPic :: Int -> [Picture]
    genLivesPic 0 = [blank]
    genLivesPic n = (translate (50 + fromIntegral n*tileSize) (-fromIntegral height/2 + 10) $ GG.png "img/snakeEast2.png") : genLivesPic (n-1)

renderMessage :: SnakeGame -> Picture
renderMessage g = pictures [countdownPic, statusMsg]
  where 
    countdownPic = if (countdownTimer g) > 0 then color white $ translate 0 10 $ scale 0.3 0.3 $ text $ show $ countdownTimer g else blank
    statusMsg    = if (gameState g) /= Playing then color white $ translate (-100) 10 $ scale 0.3 0.3 $ text msg else blank
    msg          = if (gameState g) == Won then "You won" else "Game Over"

renderLevel :: SnakeGame -> Picture
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
 | c == 's'  = translate x' y' $ color green $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == 'f'  = translate x' y' $ color red $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == '+'  = translate x' y' $ color white $ rectangleSolid (tileSize-1) 2
 | c == '.'  = translate x' y' $ color yellow $ circleSolid 2
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> SnakeGame -> SnakeGame
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g  = setSnakeDir East g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g   = setSnakeDir West g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g     = setSnakeDir North g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g   = setSnakeDir South g
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game
 | (gameState game) /= Playing = resetGameFully game
 | otherwise = game

setSnakeDir :: Direction -> SnakeGame -> SnakeGame
setSnakeDir dir g = g { snakeDir = dir } 

update :: Float -> SnakeGame -> SnakeGame
update secs game
 | (paused game)               = game
 | (gameState game) /= Playing = game
 | (countdownTimer game) > 0   = onTick game True 4 (decrementCountdown $ updateSeconds game) (updateSeconds $ game)
 | otherwise                   = checkGameState $ updateSnake $ updateSeconds game

checkGameState g
 | wallCollision = g { gameState = Lost }
 | otherwise = g
  where
   wallCollision = or $ map (\(x, y) -> getTile x y g == 'x') ts
   ts = snakeTiles g

updateSeconds :: SnakeGame -> SnakeGame
updateSeconds game = game {seconds = (seconds game) + 1, scaredTimer = (scaredTimer game) + 1}

decrementCountdown :: SnakeGame -> SnakeGame
decrementCountdown game = game {countdownTimer = (countdownTimer game) - 1}

--updateScore :: SnakeGame -> SnakeGame
--updateScore g
--  | tile == '.'      = setBlankTile $ g { score = s + 10 }
--  | tile == 'o'      = setBlankTile $ g { score = s + 50 }
--  | otherwise        = g
--  where
--    (x, y) = snakePos g
--    s = score g
--    tile = getTile x y g
--    setBlankTile = setTile x y '_'

updateSnake :: SnakeGame -> SnakeGame
updateSnake g 
 | foodCollision = g { foodPos = randomPos f, snakeTiles = growSnake f (snakeTiles g) }
 | otherwise = g { snakeTiles = updateSnakeTiles (snakeTiles g) }
  where 
    f = foodPos g
    dir = snakeDir g
    headPos = (snakeTiles g) !! 0 
    foodCollision = (move headPos dir) == f
    updateSnakeTiles ts = (move headPos dir) : init ts
    growSnake f ts = f : ts

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) None = (x, y)
move (x, y) East = (wrapx $ x+1, y)
move (x, y) West = (wrapx $ x-1, y)
move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)

canMove ::  (Int, Int) -> Direction -> SnakeGame -> Bool
canMove _ None _ = False
canMove (x, y) dir g = canMoveTo g dir $ move (x, y) dir

canMoveTo :: SnakeGame -> Direction -> (Int, Int) -> Bool
canMoveTo g dir (x, y) = getTile x y g /= 'x'

wrapx :: Int -> Int
wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

resetGame :: SnakeGame -> SnakeGame
resetGame g = g { snakeDir = snakeInitialDir, snakeTiles = snakeInitialTiles, seconds = 0, scaredTimer = 0, countdownTimer = 3}

resetGameFully :: SnakeGame -> SnakeGame
resetGameFully g = resetGame $ g {gameState = Playing, lives = snakeInitialLives, score = 0, level = (initialLevel g)}

initTiles = do 
  contents <- readFile "snake.lvl"
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, initialLevel = rows, snakeDir = snakeInitialDir, snakeTiles = snakeInitialTiles, foodPos = foodInitialPos, score = 0, seconds = 0, lives = snakeInitialLives, gen = stdGen, scaredTimer = 0, paused = False, countdownTimer = 3, gameState = Playing }
  print rows
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
