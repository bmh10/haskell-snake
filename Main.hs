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
snakeInitialTiles = [(5,10), (4,10), (3,10)]
snakeInitialDir = East
foodInitialPos = (3,3)
window = InWindow "Snake" (width, height) (offset, offset)
background = black

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data GameState = Playing | Won | Lost deriving (Eq, Show) 

randomPos game = if getTile x y game == '_' then (x, y, g') else randomPos game
  where
    g = gen game 
    (x, y, g') = randomPos' g
        

randomPos' g = (x, y, g'')
  where (x, g')  = randomR (1,26) g
        (y, g'') = randomR (1,28) g'

data SnakeGame = Game
  { 
    levelIdx :: Int,
    allLevels :: [[String]],
    currentLevel :: [String],
    snakeDir :: Direction,       -- Snake's direction of travel
    snakeTiles :: [(Int, Int)],
    foodPos :: (Int, Int),
    score :: Int,                -- Current score 
    seconds :: Float,            -- Game timer
    gen :: StdGen,               -- Random number generator
    paused :: Bool,              -- Paused or not
    countdownTimer :: Int,       -- Start of game timer
    gameState :: GameState       -- State of the game
  } deriving Show 

getLevel g = (allLevels g) !! (levelIdx g)

-- Tile functions
getTile :: Int -> Int -> SnakeGame -> Char
getTile x y g = (currentLevel g) !! y !! x

setTile :: Int -> Int -> Char -> SnakeGame -> SnakeGame
setTile x y c g = g { currentLevel = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((currentLevel g) !! y)) (currentLevel g)

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
renderDashboard g = scorePic
  where
    scorePic = color white $ translate (-30) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Score: " ++ (show $ score g)

renderMessage :: SnakeGame -> Picture
renderMessage g = pictures [countdownPic, statusMsg]
  where 
    countdownPic = if (countdownTimer g) > 0 then color white $ translate 0 10 $ scale 0.3 0.3 $ text $ show $ countdownTimer g else blank
    statusMsg    = if (gameState g) /= Playing then color white $ translate (-100) 10 $ scale 0.3 0.3 $ text msg else blank
    msg          = if (gameState g) == Won then "You won" else "Game Over"

renderLevel :: SnakeGame -> Picture
renderLevel game = renderLines (currentLevel game) 0

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
 | (gameState game) == Won = nextLevel game
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
 | wallCollision || selfCollision = g { gameState = Lost }
 | (score g) == 2 = g { gameState = Won }
 | otherwise = g
  where
   wallCollision = or $ map (\(x, y) -> getTile x y g == 'x') ts
   selfCollision = (length $ nub ts) /= length ts
   ts = snakeTiles g

updateSeconds :: SnakeGame -> SnakeGame
updateSeconds game = game {seconds = (seconds game) + 1}

decrementCountdown :: SnakeGame -> SnakeGame
decrementCountdown game = game {countdownTimer = (countdownTimer game) - 1}

updateSnake :: SnakeGame -> SnakeGame
updateSnake g 
 | foodCollision = g { foodPos = (fx, fy),
                       gen = g', 
                       snakeTiles = growSnake f (snakeTiles g),
                       score = (score g) + 1 }
 | otherwise = g { snakeTiles = updateSnakeTiles (snakeTiles g) }
  where
    (fx, fy, g') = randomPos g 
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

nextLevel g
 | nextIdx >= numLevels = resetGameFully g
 | otherwise = resetGameFully $ g { levelIdx = nextIdx, currentLevel = nextLevel }
  where
    nextIdx = (levelIdx g) + 1
    numLevels = length (allLevels g)
    nextLevel = (allLevels g) !! nextIdx

resetGame :: SnakeGame -> SnakeGame
resetGame g = g { snakeDir = snakeInitialDir, snakeTiles = snakeInitialTiles, foodPos = foodInitialPos, seconds = 0, countdownTimer = 3}

resetGameFully :: SnakeGame -> SnakeGame
resetGameFully g = resetGame $ g {gameState = Playing, score = 0}

initTiles = do 
  let fileNames = map (\x -> "snake" ++ show x ++ ".lvl") [1..3]
  fileContents <- mapM readFile fileNames
  let all = map words fileContents
  stdGen <- newStdGen
 
  let initialState = Game { allLevels = all, currentLevel = all !! 0, levelIdx = 0, snakeDir = snakeInitialDir, snakeTiles = snakeInitialTiles, foodPos = foodInitialPos, score = 0, seconds = 0, gen = stdGen, paused = False, countdownTimer = 3, gameState = Playing }
  print all
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
