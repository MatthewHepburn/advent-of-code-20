import System.IO
import Data.List.Split

main = do
    handle <- openFile "input" ReadMode
    fileContents <- hGetContents handle
    let inputLines = filter (\y -> length y > 0) (lines fileContents)
    let inputMap =  map cycle inputLines

    let visitedPositions = reverse $ getPointsOnTrajectory (length inputMap - 1) (1, 3) [(0,0)]
    let visitedTiles = getTiles inputMap visitedPositions []
    let solution = sum [ 1 | tile <- visitedTiles, tile == '#']

    print solution
    hClose handle

type Tile = Char
type Row = [Tile]
type Map = [Row]
type Trajectory = (Int, Int)
type Position = (Int, Int)


getPointsOnTrajectory:: Int ->  Trajectory -> [Position] -> [Position]
getPointsOnTrajectory 0 _ positions = positions
getPointsOnTrajectory n trajectory  (lastPos:positions) = do 
    let newPos = (fst trajectory + fst lastPos, snd trajectory + snd lastPos)
    getPointsOnTrajectory (n - 1) trajectory (newPos:lastPos:positions)

getTiles:: Map -> [Position] -> [Tile] -> [Tile]
getTiles _ [] tiles = tiles
getTiles map (position:positions) tiles = do
    let targetRow = map !! (fst position)
    let newTile = targetRow !! (snd position)
    getTiles map positions (newTile:tiles)
