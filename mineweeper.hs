import Data.Char
import System.Exit
-- import System.Console.ANSI
import Graphics.Vty
-- import System.Random

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
newGame r c = (newBoard r c, newBoard r c)


newBoard :: Int -> Int -> [[Int]]
newBoard rows columns = take rows (repeat (take columns (repeat 0)))


-- (-1) represents bomb, have to calculate actual values
boardTest :: [[Int]]
boardTest = [[0, 0, 0,  0, 0,  0],
             [0, 0, 0,  0, 0, -1],
             [0, 0, 0, -1, 0,  0],
             [0, 0, 0,  0, 0,  0],
             [0, 0, 0,  0, 0,  0],
             [1, 2, 3,  4, 5,  6]]



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
    | x ==  0 = '\2588':""
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



-- TODO check if click-ed spot is a part of the board
-- TODO Make board logic


-- Translates terminal-coordinates to game-board coordinates (if possible)
translateCoordinate :: Pos -> Maybe Pos
translateCoordinate (x, y)
    | mod (x - 1) 2 == 0 && y >= header = Just (div (x - margin_left) 2 , div (y - header) 1)
    | otherwise = Nothing




revealBoard :: Game -> Pos -> Game
revealBoard (shown, hidden) (x, y) = undefined




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
    renderBoard (shown, hidden) vty (unwords $ (map show [x_mouse, y_mouse]) ++ [show button])
    event <- nextEvent vty
    mainLoop event vty (shown, hidden) (x, y)

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
            let board = (boardTest, boardTest)  -- (newBoard 6 6,boardTest) --makeBoardText rows columns
            print board

            cfg <- standardIOConfig
            vty <- mkVty cfg

            let output = outputIface vty
            --putStrLn $ show output
            setMode output Mouse True
            -- TODO -initiate board here-
    
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