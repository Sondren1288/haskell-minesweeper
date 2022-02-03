import Data.Char
import System.Exit
-- import System.Console.ANSI
import Graphics.Vty
import Data.List
import System.Random

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
newGame :: Int -> Int -> Game
newGame r c = (newShown r c, newHidden r c)


-- 
newShown :: Int -> Int -> [[Int]]
newShown rows columns = take rows (repeat (take columns (repeat (-3))))

newHidden :: Int -> Int -> [[Int]]
newHidden _ _ = calculateHidden boardTest
--newHidden rows columns = take rows (repeat (take columns (repeat (0))))

calculateHidden :: [[Int]] -> [[Int]]
calculateHidden board = [[if c == (-1) then c else countBombs board (x, y) | (c, x) <- (zip r [0..])] | (r, y) <- (zip board [0..])]

countBombs :: [[Int]] -> Pos -> Int
countBombs board (0, 0) =   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (take 2 board)]
countBombs board (0, y)
    | y == length board =   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (drop ((length board)-2) board)]
    | otherwise =           sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 r] | r <- (take 3 (drop (y-1) board))]
countBombs board (x, 0)
    | x == length (head board) =    sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (take 2 board)]
    | otherwise =                   sum $ concat [[if c == (-1) then 1 else 0 | c <- take 3 (drop (x-1) r)] | r <- (take 2 board)]
countBombs board (x, y) 
    | x == length (head board) && y == length board = sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (drop ((length board)-2) board)]
    | x == length (head board) =    sum $ concat [[if c == (-1) then 1 else 0 | c <- take 2 (drop (x-1) r)] | r <- (take 3 (drop (y-1) board))]
    | y == length board =           sum $ concat [[if c == (-1) then 1 else 0 | c <- take 3 (drop (x-1) r)] | r <- (drop ((length board)-2) board)]
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



{-
generateRandomNumber :: Int -> Int 
generateRandomNumber upper_bound = do
    g <- newStdGen
    integer <- head $ randomRs (0, upper_bound) g
    return integer
-}

{--
generateBombs :: Int -> Int -> Int -> ([Int], [Int]) 
generateBombs rows columns nBombs = do
    g <- newStdGen
    integerList <- zip (take nBombs (randomRs (0, rows) g)) (take nBombs (randomRs (0, columns) g))  
    return integerList
--}


formatInt :: Int -> String
formatInt x
    | x == -1 = "b"
    | x == -2 = "f"
    | x == -3 = '\2658':""
    | x ==  0 = ' ':""
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
insertMutlipleToGrid (x:xs) (shown, hidden) = insertMutlipleToGrid xs (insertIntoGrid shown x (getValue hidden x), hidden)


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
    | otherwise = (shown, hidden)

hasOneEqual :: (Eq a) => [a] -> [a] -> [Bool]
hasOneEqual list1 list2 = [elem x list2 | x <- list1]


mainLoop :: Event -> Vty -> Game -> Pos -> IO ()
mainLoop (EvKey key m) vty (shown, hidden) (x, y) = do
    if key == KChar 'q' then do
        shutdown vty
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



makeBoardText :: String -> String -> Game
makeBoardText rows columns = newGame (read (rows) :: Int) (read (columns) :: Int) 


rowsColsDigit :: String -> String -> Bool
rowsColsDigit rows columns
    | all (==True) (map (all (isDigit)) [rows, columns]) = True
    | otherwise = False



main :: IO ()
main  = do
    clear_screen
    home
    putStrLn "Rows: "
    rows <- getLine
    putStrLn "columns: "
    columns <- getLine

    if not (rowsColsDigit (unwords $ words rows) (unwords $ words columns))
        then do
            putStrLn "Row and columns need to be numbers"
            exitWith (ExitFailure 1)
    else do
            let board = (newShown 6 6, calculateHidden boardTest)  -- (newBoard 6 6,boardTest) --makeBoardText rows columns
            print board

            cfg <- standardIOConfig
            vty <- mkVty cfg

            let output = outputIface vty
            --putStrLn $ show output
            setMode output Mouse True
            -- TODO -initiate board here-
            
            renderBoard board vty ("Top")

            event <- nextEvent vty
            mainLoop event vty board (3, 3)
            shutdown vty

    --mainLoop (nextEvent vty) vty
    {-
    cfg <- standardIOConfig --customIOConfig
    vty <- mkVty cfg
    let 
        temp = string (defAttr `withForeColor` white) "Apple bottom jeans"
        pic = picForImage temp
    update vty pic
    e <- nextEvent vty
    mainLoop e
    -- print ("Last event was: " ++ show e)
    -}

    -- Test field
    {-let 
        temp = string (defAttr `withForeColor` white) "Apple bottom jeans"
        pic = picForImage temp
    update vty pic
    e <- nextEvent vty
    let 
        temp = string standardAttribute (show e)
        pic = picForImage temp
    update vty pic
    e <- nextEvent vty
    let
        temp = string standardAttribute (show $ type e)
        pic = picForImage temp
    update vty pic
    e <- nextEvent vty-}

