import Data.Char
import System.Exit
-- import System.Console.ANSI
import Graphics.Vty
import Data.List
import System.Random
-- lacking critical imports here for program to function
-- GetCursorPosition returns cursor position

-- For easier type declarations
type Game = ([[Int]], [[Int]])
-- Int represents numbre of bombs in nearby grid, while -1 represents bomb
type Pos = (Int, Int)


-- Constants
header :: Int
header = 2

margin_left :: Int
margin_left = 1

tile_size :: Int
tile_size = 1

divisor_size_vertical :: Int
divisor_size_vertical = 0

divisor_size_horizontal :: Int
divisor_size_horizontal = 1

cell_size_vertical :: Int
cell_size_vertical = 1

cell_size_horizontal :: Int
cell_size_horizontal = 1


-- terminal indexing starts at 0, 0, upper left corner


-- ####################################### Pretty Printing
-- For nicer printing
clear_screen :: IO ()
clear_screen = putStr "\ESC[3J\ESC[2J" 

-- To return cursor position upper left corner
home :: IO ()
home = putStr "\ESC[1;1H"


-- Move to the specified position
goto :: Int -> Int -> IO ()
goto x y = putStr $ "\ESC[" ++ show (y) ++ ";" ++ show (x) ++ "H"

standardAttribute :: Attr
standardAttribute = defAttr `withForeColor` white


{-
- Left for revealed state
- Right for state of the game beneath the surface
-}
--newGame :: Int -> Int -> Int -> StdGen -> Game
--newGame r c bombs gen = (newShown r c, newHidden r c bombs gen)


-- 
newShown :: Int -> Int -> [[Int]]
newShown rows columns = take rows (repeat (take columns (repeat (-3))))

newHidden :: Int -> Int -> Int -> StdGen -> StdGen -> [[Int]]
newHidden r c bombs gen1 gen2 = calculateHidden $ insertBombsToGrid (generateBombs (newBlank r c) bombs gen1 gen2) (newBlank r c)

newBlank :: Int -> Int -> [[Int]]
newBlank rows columns = take rows (repeat (take columns (repeat (0))))


calculateHidden :: [[Int]] -> [[Int]]
calculateHidden board = [[if c == (-1) then c else countBombs board (x, y) | (c, x) <- (zip r [0..])] | (r, y) <- (zip board [0..])]

countBombs :: [[Int]] -> Pos -> Int
countBombs board (0, 0) =   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (take 2 board)]
countBombs board (0, y)
    | y == length board =   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (drop ((length board)-1) board)]
    | otherwise =           sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (take 3 (drop (y-1) board))]
countBombs board (x, 0)
    | x == length (head board) =    sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (take 2 board)]
    | otherwise =                   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 3 (drop (x-1) r)] | r <- (take 2 board)]
countBombs board (x, y) 
    | x == length (head board) && y == length board = sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (drop ((length board)-1) board)]
    | x == length (head board) =    sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (take 3 (drop (y-1) board))]
    | y == length board =           sum $ concat [[if c == (-1) then 1 else 0 | c <- take 3 (drop (x-1) r)] | r <- (drop ((length board)-1) board)]
    | otherwise =                   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 3 (drop (x-1) r)] | r <- (take 3 (drop (y-1) board))]


-- (-1) represents bomb, have to calculate actual values
-- (-3) are exclusive to the rendered board and represents an un-turned value
boardTest :: [[Int]]
boardTest = [[-3,-1, 0,  0, 0,  0],
             [-3, 0, 0,  0, 0, -1],
             [-3, 0, 0, -1, 0,  0],
             [-1, 0, 0,  0, 0,  0],
             [ 0, 0, 0,  0, 0,  0],
             [ 1, 2, 3,  4, 5,  6]]


generateRandomNumbers :: Int -> Int -> Int -> StdGen -> [Int]
generateRandomNumbers lower upper amount g = take amount (randomRs (lower, upper) g)

generateBombs :: [[Int]] -> Int -> StdGen -> StdGen -> [Pos]
generateBombs board nBombs gen1 gen2 = zip (generateRandomNumbers 0 ((length $ head board)-1) nBombs gen1) (generateRandomNumbers 0 ((length board)-1) nBombs gen2)

insertBombsToGrid :: [Pos] -> [[Int]] -> [[Int]]
insertBombsToGrid [] board = board
insertBombsToGrid (x:xs) board = insertBombsToGrid xs (insertIntoGrid board x (-1))


resetColor :: String
resetColor = "\ESC[0m"


formatInt :: Int -> String
formatInt x
    | x == -1 = "¤"  -- ++ resetColor
    | x == -2 = "f"  -- ++ resetColor
    | x == -3 = "#"  -- ++ resetColor
    | x ==  0 = " "  -- ++ resetColor
    | x ==  1 = "1"  -- "\ESC[94;40m" ++ "1" ++ resetColor
    | x ==  2 = "2"  -- "\ESC[32;40m" ++ "2" ++ resetColor
    | x ==  3 = "3"  -- "\ESC[91;40m" ++ "3" ++ resetColor
    | x ==  4 = "4"  -- "\ESC[34;40m" ++ "4" ++ resetColor
    | x ==  5 = "5"  -- "\ESC[33;40m" ++ "5" ++ resetColor
    | x ==  6 = "6"  -- "\ESC[36;40m" ++ "6" ++ resetColor
    | x ==  7 = "7"  -- "\ESC[34;40m" ++ "7" ++ resetColor
    | x ==  8 = "8"  -- "\ESC[90;40m" ++ "8" ++ resetColor
    | otherwise = show x 



prettyFormatList :: [[Int]] -> [String]
prettyFormatList list = seperator ++ [concat ["|" ++ (formatInt c) | c <- r] ++ "|" | r <- list] ++ seperator
                        where 
                            seperator = [concat ["-" | _ <- [0..(length $ head list) * 2]]]



foldImage :: [String] -> Image
foldImage (x:xs) = foldl (vertJoin) (string standardAttribute x) (map (string standardAttribute) (xs))


renderBoard :: Game -> Vty -> String -> IO ()
renderBoard (shown, hidden) vty text = do
    let renderImage = foldImage (text : prettyFormatList shown)
        pic = picForImage renderImage
    update vty pic



getValue :: [[Int]] -> Pos -> Int
getValue board (x, y) = last $ take (x+1) (last $ take (y+1) board) 


-- Translates terminal-coordinates to game-board coordinates (if possible)
translateCoordinate :: Pos -> Game -> Maybe Pos
translateCoordinate (x, y) (shown, hidden)
    | x >= margin_left + (divisor_size_horizontal + cell_size_horizontal) * (length $ head shown) = Nothing
    | y >= header      + (divisor_size_vertical   + cell_size_vertical)   * (length shown)        = Nothing
    | mod (x - 1) 2 == 0 && y >= header = Just (div (x - margin_left) (divisor_size_horizontal + cell_size_horizontal) , div (y - header) (divisor_size_vertical + cell_size_vertical))
    | otherwise = Nothing


checkTile :: Game -> Pos -> Game
checkTile (shown, hidden) (x, y)
    | getValue shown (x, y) == -3 = 
        if getValue hidden (x, y) == 0 
                            --  [ insertIntoGrid | pos <- revelaQueue [] [(x, y)] (shown, hidden)]]
            then insertMutlipleToGrid ((revealQueue [] [(x, y)] (shown, hidden)) ++ (nub $ concat $ map (checkBorderTile (shown, hidden) [1..9]) (revealQueue [] [(x, y)] (shown, hidden)))) (shown, hidden)  -- (insertIntoGrid shown (x, y) (getValue hidden (x, y)), hidden) -- ((take (x)) . (last $ take (y+1))) shown ++ getValue hidden (x, y) ++ 
        else
            (insertIntoGrid shown (x, y) (getValue hidden (x, y)), hidden)
    | otherwise = (shown, hidden)

insertMutlipleToGrid :: [Pos] -> Game -> Game
insertMutlipleToGrid [] game = game
insertMutlipleToGrid (x:xs) (shown, hidden) = 
    if getValue shown x == -3 
        then insertMutlipleToGrid xs (insertIntoGrid shown x (getValue hidden x), hidden)
    else insertMutlipleToGrid xs (shown, hidden)

-- Finds all 0-es. The return-list needs to be run through again so that it finds all non-zero
-- nub removes all duplicate elements of the list
revealQueue :: [Pos] -> [Pos] -> Game -> [Pos]
revealQueue posList [] (shown, hidden) = posList
revealQueue posList newPosList (shown, hidden) = revealQueue (posList ++ newPosList) (nub [newPos | newPos <- concat $ map (checkBorderTile (shown, hidden) [0]) newPosList, notElem newPos posList]) (shown, hidden) 


-- Checks the values of all the 4 border-tiles and checks if it is part of vals
checkBorderTile :: Game -> [Int] -> Pos -> [Pos]
checkBorderTile (shown, hidden) vals (x, y) = concat [ifFirstReturnSecond vals posList | posList <- zip (map (getValue hidden) neighbours) neighbours] -- [(Int, Pos)]
    where neighbours = getValidNeighboursCross shown (x, y) :: [Pos]


-- if a is an element of vals then return b
ifFirstReturnSecond :: [Int] -> (Int, Pos) -> [Pos]
ifFirstReturnSecond vals (a, b) = if elem a vals then [b] else []



getValidNeighboursCross :: [[Int]] -> Pos -> [Pos]
getValidNeighboursCross board (0, 0) = [(1, 0), (0, 1), (1, 1)]
getValidNeighboursCross board (0, y) 
    | y == length board - 1 = [          (0, y-1), (1, y), (1, y-1)]
    | otherwise =             [(0, y+1), (0, y-1), (1, y), (1, y-1), (1, y+1)]
getValidNeighboursCross board (x, 0)
    | x == length (head board) - 1  = [(x-1, 0),           (x, 1), (x-1, 1)]
    | otherwise =                     [(x-1, 0), (x+1, 0), (x, 1), (x-1, 1), (x+1, 1)]
getValidNeighboursCross board (x, y) 
    | x == length (head board) - 1 && y == length board - 1= [(x-1, y), (x, y-1), (x-1, y-1)]
    | x == length (head board) - 1 = [(x-1, y),           (x, y-1), (x, y+1), (x-1, y+1), (x-1, y-1)]
    | y == length board - 1 =        [(x-1, y), (x+1, y), (x, y-1), (x+1, y-1), (x-1, y-1)]
    | otherwise =                    [(x-1, y), (x+1, y), (x, y+1), (x, y-1), (x-1, y+1), (x-1, y-1), (x+1, y+1), (x+1, y-1)]



-- Inserts a number into the grid by taking all elements until the element, adding the element, and then taking all elements after the element
insertIntoGrid :: [[Int]] -> Pos -> Int -> [[Int]]
insertIntoGrid board (x, y) val = take (y) board ++ [take x (last $ take (y+1) board) ++ [val] ++ drop (x+1) (last $ take (y + 1) board)] ++ drop (y+1) board



revealBoard :: Game -> Pos -> Game
revealBoard (shown, hidden) (x, y) =
    case (translateCoordinate (x, y) (shown, hidden)) of 
        Nothing -> (shown, hidden)
        Just (x1, y1) -> checkTile (shown, hidden) (x1, y1)

plantFlag :: Game -> Pos -> Game
plantFlag (shown, hidden) (x, y) = 
    case (translateCoordinate (x, y) (shown, hidden)) of
        Nothing -> (shown, hidden)
        Just (x1, y1) -> checkTileFlag (shown, hidden) (x1, y1)


checkTileFlag :: Game -> Pos -> Game
checkTileFlag (shown, hidden) (x, y)
    | getValue shown (x, y) == -3 = (insertIntoGrid shown (x, y) (-2), hidden)
    | getValue shown (x, y) == -2 = (insertIntoGrid shown (x, y) (-3), hidden)
    | otherwise = (shown, hidden)

hasOneEqual :: (Eq a) => [a] -> [a] -> [Bool]
hasOneEqual list1 list2 = [elem x list2 | x <- list1]


-- TODO Win game
-- TODO Lose game
-- TODO Better Header

mainLoop :: Event -> Vty -> Game -> Pos -> IO ()
mainLoop (EvKey key m) vty (shown, hidden) (x, y) = do
    if key == KChar 'q' then do
        shutdown vty
        --print (shown, hidden)
        exitWith (ExitSuccess)
    else do
        renderBoard (shown, hidden) vty (show key)
        event <- nextEvent vty
        mainLoop event vty (shown, hidden) (x, y)
mainLoop (EvMouseDown x_mouse y_mouse button modifiers) vty (shown, hidden) (x, y) = do
    case modifiers of [MAlt] -> do
                            let newGame = plantFlag (shown, hidden) (x_mouse, y_mouse)
                            renderBoard newGame vty (unwords $ (map show [x_mouse, y_mouse]) ++ [show button, show modifiers])
                            event <- nextEvent vty
                            mainLoop event vty newGame (x, y)
                      [MCtrl] -> do
                            let newGame = plantFlag (shown, hidden) (x_mouse, y_mouse)
                            renderBoard newGame vty (unwords $ (map show [x_mouse, y_mouse]) ++ [show button, show modifiers])
                            event <- nextEvent vty
                            mainLoop event vty newGame (x, y)
                      otherwise -> do 
                            let newGame = revealBoard (shown, hidden) (x_mouse, y_mouse)
                            renderBoard newGame vty (unwords $ (map show [x_mouse, y_mouse]) ++ [show button])
                            event <- nextEvent vty
                            mainLoop event vty newGame (x, y)
                

-- mainLoop (EvMouseUp x_mouse y_mouse button modifiers) vty (shown, hidden) (x, y)
mainLoop event vty (shown, hidden) (x, y) = do -- Default event when not a key or a mousebuuton : do nothing
    e <- nextEvent vty
    mainLoop e vty (shown, hidden) (x, y)


-- parser :: Event -> Vty ->
parser (EvKey x m) vty = 
    if x == KChar 'q' 
        then do
            shutdown vty
            exitWith (ExitSuccess)
    else
        case x of 
            KUp -> undefined -- Move up
            KDown -> undefined -- Move down
            KLeft -> undefined -- Move left
            KRight -> undefined -- Move right
            KEnter -> undefined -- Plant flag?
            KChar ' ' -> undefined -- Open tile?
            {-
            otherwise -> if elem x (map (KChar) ['0'..'9'])
                then do
                    let list_ = takeWhile isDigit (phantom vty) -- is digit, do the thing
                        temp = string standardAttribute (show list_)
                        pic = picForImage temp
                    update vty pic
                else undefined
-}



--makeBoardText :: String -> String -> Game
--makeBoardText rows columns = newGame (read (rows) :: Int) (read (columns) :: Int) 


rowsColsDigit :: String -> String -> String -> Bool
rowsColsDigit rows columns bombs
    | all (==True) (map (all (isDigit)) [rows, columns, bombs]) = True
    | otherwise = False



main :: IO ()
main  = do
    clear_screen
    home
    putStrLn "Rows: "
    rows <- getLine
    putStrLn "columns: "
    columns <- getLine
    putStrLn "Percentage bombs: "
    bombs <- getLine

    if not (rowsColsDigit (unwords $ words rows) (unwords $ words columns) (unwords $ words bombs))
        then do
            putStrLn "Row and columns need to be numbers"
            exitWith (ExitFailure 1)
    else do
            let   -- (newBoard 6 6,boardTest) --makeBoardText rows columns
                r = read rows :: Int
                c = read columns :: Int
                b = read bombs :: Int
                total_bombs = if (r * c * b) `div` 100 > 0 then (r * c * b) `div` 100  else 0
                board = (newShown r c, newShown r c)

            cfg <- standardIOConfig
            vty <- mkVty cfg

            -- Enables Mouse support
            let output = outputIface vty
            setMode output Mouse True
            
            -- Renders dummy-board for first click
            renderBoard board vty ("Top")
            -- Get first click 
            event <- nextEvent vty
            dummyLoop r c total_bombs vty board event
            --mainLoop event vty board (3, 3)
            shutdown vty


dummyLoop r c total_bombs vty board (EvMouseDown x_mouse y_mouse button modifiers) = do
    case (translateCoordinate (x_mouse, y_mouse) board) of 
        Just (x1, y1) -> do
            gen1 <- newStdGen
            gen2 <- newStdGen
            let new_hidden = calculateHidden $ insertIntoGrid (newHidden r c total_bombs gen1 gen2) (x1, y1) 0
            e <- nextEvent vty
            mainLoop e vty (fst board, new_hidden) (0, 0)
        Nothing -> do
            e <- nextEvent vty
            dummyLoop r c total_bombs vty board e
dummyLoop r c total_bombs vty board event = do
    e <- nextEvent vty
    dummyLoop r c total_bombs vty board event
