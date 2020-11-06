import Data.Char
import System.Random

w = 8
h = 8
r = h*w
mines = 10
-- STARTING

x minesplaces2 = qsort minesplaces2

minesplaces minesplaces2 = helpmin 1 (x minesplaces2)
helpmin :: Int->[Int]->[Int]
helpmin n [] = [0 |xs<-[n..r]]
helpmin n _ | n > r = []
helpmin n (x:xs) | (n==x) = 1:helpmin (n+1) xs
                 | otherwise = 0:helpmin (n+1) (x:xs)

qsort::[Int]->[Int]
qsort [] = []
qsort (x:xs) = qsort small ++ [x] ++ qsort large 
            where small = [y | y<-xs, y<=x]    
                  large = [y | y<-xs, y>x]

removeDuplicates::[Int]->[Int] 
removeDuplicates []  = [] 
removeDuplicates [x] = [x] 
removeDuplicates (x:y:xs)     
    | x == y         = x:removeDuplicates xs       
    | otherwise      = x:removeDuplicates (y:xs)  

randomList::Int->Int->Int->Int->[Int]
randomList n sgn start end 
    | (end - start) < 0 = []
    | (end - start+1) < n = []
    | (length (removeDuplicates (qsort list))) < n = randomList n (sgn+1) start end
    | otherwise = list
    where list = take n $ randomRs (start,end) (mkStdGen sgn)  :: [Int]


numbers :: [Int]->[Int]
numbers [] = []
numbers (x:l) | (x == 1) = (x+1):(x+w):(x+w+1):numbers l --top left corner
              | (x == w) = (x-1):(x+w):((x+w)-1):numbers l --top right corner 
              | (x == r) = (x-w):(x-1):((x-w)-1):numbers l --bottom right corner
              | (x == ((r - w)+1)) = (x+1):(x-w):((x-w)+1):numbers l --bottom left corner
              | (mod x w) == 0 = (x-1):(x-w):(x+w):((x-w)-1):((x+w)-1):numbers l --right border
              | (mod x w) == 1 = (x+1):(x-w):(x+w):((x-w)+1):((x+w)+1):numbers l--left border
              | (x - w) < 1 = (x+1):(x-1):(x+w):((x+w)+1):((x+w)-1):numbers l --top border
              | (x + w) > r = (x+1):(x-1):(x-w):((x-w)+1):((x-w)-1):numbers l--bottom border
              | otherwise = (x+1):(x-1):(x-w):(x+w):((x-w)+1):((x-w)-1):((x+w)+1):((x+w)-1):numbers l
  
numsort minesplaces2 = qsort (numbers minesplaces2)

num :: [Int]->[(Int,Int)]
num numsorted = helpnum 1 0 numsorted

helpnum :: Int->Int->[Int]->[(Int,Int)]
helpnum i _ _ | i > r = []
helpnum i v [] = (i,v):helpnum (i+1) 0 []        
helpnum i v (x:l) | (i == x) = helpnum i (v+1) l
                  | otherwise = (i,v):helpnum (i+1) 0 (x:l) 

start :: [Int]->[(Int,Int)]->[(Int,Int)]
start mplaces number = helps 1 mplaces number 

helps :: Int->[Int]->[(Int,Int)]->[(Int,Int)]
helps n _ [] = []
helps n [] l = [(0,0) |xs<-[n..r]]
helps n _ _ | n > r = []
helps n (x:mplaces) ((y,v):num) | (x == 1) = (10,0):helps (n+1) mplaces num
                                | otherwise = (v,0):helps (n+1) mplaces num
-- END of STARTING 

try minesplaces2 = start (minesplaces minesplaces2) (num (numsort minesplaces2))  -- First list 


action :: Int->[(Int,Int)]->[(Int,Int)]
action x l | x > r = l
           | ((snd (l!!(x-1))) == 2) = unflag x l 
           | ((fst (l!!(x-1))) == 0) = fr8 x l
           | otherwise = showValue x l
           

changValue :: Int->Int->[(Int,Int)]->[(Int,Int)]
changeValue pos _ l | pos > r = l 
changValue pos v l = helpc 1 pos v l

helpc :: Int->Int->Int->[(Int,Int)]->[(Int,Int)]
helpc n _ _ _ | n > r = []
helpc n pos v ((x,y):l) | (pos == n) = (v,y):l
                        | otherwise = (x,y):helpc (n+1) pos v l

changeViewed :: Int->Int->[(Int,Int)]->[(Int,Int)]
changeViewed pos _ l | pos > r = l 
changeViewed pos v l = helpcv 1 pos v l

helpcv :: Int->Int->Int->[(Int,Int)]->[(Int,Int)]
helpcv n _ _ _ | n > r = []
helpcv n pos v ((x,y):l) | (pos == n) = (x,v):l
                         | otherwise = (x,y):helpcv (n+1) pos v l
                        
-- START of tfry8

fr8 :: Int->[(Int,Int)]->[(Int,Int)]
fr8 pos l = helpf 1 pos l l


helpf :: Int->Int->[(Int,Int)]->[(Int,Int)]->[(Int,Int)]
helpf n _ _ _ | n > r = []
helpf n pos ((x,y):l) l2 | (x == 10) || (y == 1) = (x,y):helpf (n+1) pos l l2 -- mine or showed before
                         | (haveRoad n pos l2) = (x,1):helpf (n+1) pos l l2  -- not mine and coverd
                         | otherwise = (x,y):helpf (n+1) pos l l2 -- doesn't haveRoad


-- DFS START

prev = [0 |xs<-[1..r]] -- use in DFS

haveRoad :: Int->Int->[(Int,Int)]->Bool   
haveRoad x pos l | (x<1 || x>r) = False
                 | (x == pos) = True
                 | ((fst (l!!(x-1))) == 10) = False 
                 | otherwise = check x pos (mark prev x) l 


haveRoad2 :: Int->Int->[Int]->[(Int,Int)]->Bool 
haveRoad2 x pos prev l | (prev!!(x-1) == 1) = False
                       | (x<1 || x>r) = False
                       | (x == pos) = True
                       | ((fst (l!!(x-1))) == 0) = check x pos (mark prev x) l 
                       | otherwise = False
                       

check :: Int->Int->[Int]->[(Int,Int)]->Bool 
check x pos prev l | (x == 1) = haveRoad2 (x+1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 (x+w+1) pos (mark prev x) l --top left corner
                   | (x == w) = haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 ((x+w)-1) pos (mark prev x) l --top right corner 
                   | (x == r) = haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)-1) pos (mark prev x) l --bottom right corner
                   | (x == ((r - w)+1)) = haveRoad2 (x+1) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)+1) pos (mark prev x) l --bottom left corner
                   | (mod x w) == 0 = haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)-1) pos (mark prev x) l || haveRoad2 ((x+w)-1) pos (mark prev x) l --right border
                   | (mod x w) == 1 = haveRoad2 (x+1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)+1) pos (mark prev x) l || haveRoad2 ((x+w)+1) pos (mark prev x) l --left border
                   | (x - w) < 1 = haveRoad2 (x+1) pos (mark prev x) l || haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 ((x+w)-1) pos (mark prev x) l || haveRoad2 ((x+w)+1) pos (mark prev x) l --top border
                   | (x + w) > r = haveRoad2 (x+1) pos (mark prev x) l || haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)-1) pos (mark prev x) l || haveRoad2 ((x-w)+1) pos (mark prev x) l --bottom border
                   | otherwise = haveRoad2 (x+1) pos (mark prev x) l ||  haveRoad2 (x-1) pos (mark prev x) l || haveRoad2 (x+w) pos (mark prev x) l || haveRoad2 (x-w) pos (mark prev x) l || haveRoad2 ((x-w)-1) pos (mark prev x) l || haveRoad2 ((x-w)+1) pos (mark prev x) l || haveRoad2 ((x+w)-1) pos (mark prev x) l || haveRoad2 ((x+w)+1) pos (mark prev x) l

mark :: [Int]->Int->[Int]
mark l pos = helpmark 1 l pos

helpmark :: Int->[Int]->Int->[Int]
helpmark n _ _ | n > r = []
helpmark n (x:l) pos | n == pos = 1:l
                     | otherwise = x:helpmark (n+1) l pos   
                     
-- DFS END

-- END of tfry8
                      
-- START of showValue

showValue :: Int->[(Int,Int)]->[(Int,Int)]
showValue pos l = changeViewed pos 1 l
                      
-- END of showValue      

-- START check if win or lose

isWin :: [(Int,Int)]->Bool
isWin l = numberOfShowed l == (r-mines)

numberOfShowed :: [(Int,Int)]->Int
numberOfShowed [] = 0
numberOfShowed ((x,y):l) | (x < 10) && (y == 1) = (1 + numberOfShowed l)
                         | otherwise = numberOfShowed l
                         
                         
isLose :: [(Int,Int)]->Bool 
isLose l = helpisLose 1 l

helpisLose ::  Int->[(Int,Int)]->Bool
helpisLose n _ | n > r = False
helpisLose n ((x,y):l) | (x == 10) && (y == 1) = True
                       | otherwise = helpisLose (n+1) l
                         
                
-- END check if win or lose                         
                         
                         
showAll :: [(Int,Int)]->[(Int,Int)]
showAll [] = []
showAll ((x,y):l) = (x,1):showAll l

------------------------------------------------------------
--Start of Drawing Functions
list2Lines::(Show a)=>[a]->IO ()
list2Lines l =  putStr (unlines $ (map show l))

divideList::Int->[a]->[[a]]
divideList _ [] = []
divideList w list
    | w <= 0 = []
divideList w list = take w list : (divideList w (drop w list))

-- set Flag

f :: Int->[(Int,Int)]->[(Int,Int)]
f pos l |pos > r = l
        |(snd(l!!(pos-1))) == 1 = l
        |otherwise = changeViewed pos 2 l

-- set Flag

-- unset flag
unflag :: Int->[(Int,Int)]->[(Int,Int)]
unflag pos l = changeViewed pos 0 l

-- unset flag


generateList::[(Int,Int)]->[Char]
generateList [] = []
generateList ((value,viewed):tuples)
    |viewed == 2 = ' ':'!':' ':               generateList tuples
    |viewed == 0 = ' ':'#':' ':               generateList tuples
    |value == 10 = ' ':'*':' ':               generateList tuples
    |value == 0  = ' ':' ':' ':               generateList tuples
    |otherwise   = ' ':(chr (value+48)):' ':  generateList tuples

------------------Start Adding Help Numbers------------------
replaceElement::a->Int->[a]->[a]
replaceElement element pos l = (take (pos-1) l) ++ (element:(drop pos l))
insertElement ::a->Int->[a]->[a]
insertElement  element pos l = (take pos l) ++ (element:(drop pos l))

addHorizontalHelpingNumbers::Int->[Char]
addHorizontalHelpingNumbers n 
    | n > w    = []
    |otherwise =  '-' : char : '-' : addHorizontalHelpingNumbers (n+1)
    where char = if n==0 then '-' else (chr (n+48))
    
addVerticalHelpingNumbers::[Char]->Int->[Char]
addVerticalHelpingNumbers l n
    | n >= h    = hold3
    |otherwise = addVerticalHelpingNumbers hold3 (n+1)     
    where hold1 = insertElement '-'          offset    l
          hold2 = insertElement (chr (n+48)) offset  hold1
          hold3 = insertElement '-'          offset  hold2
          offset= (3*w+3)*(n-1) --3*w*(n-1)+3*(n-1)
    
addHelpingNumbers::[Char]->[Char]
addHelpingNumbers l =  addHorizontalHelpingNumbers 0 ++ addVerticalHelpingNumbers l 1
------------------End   Adding Help Numbers------------------

drawBoard ::[(Int,Int)]->IO ()
drawBoard l = list2Lines (divideList (3*(w+1)) (addHelpingNumbers(generateList l)))

getPos :: (Int,Int)->Int
getPos (x,y) = ((x-1) * w) +y

play ::[(Int,Int)]->IO()
play l = do
       --putStr "\ESC[2J" -- work only if we load the code in cmd
       if (isLose l) then do
            drawBoard (showAll l)
            putStrLn "YOU LOSE"
            n <- getLine 
            putStrLn "BYE BYE"   
       else if (isWin l) then do
            drawBoard (showAll l)
            putStrLn "YOU WIN"
            n <- getLine 
            putStrLn "BYE BYE"  
       else do
            drawBoard l
            move <- getLine 
            if (((length move) /= 3)||(((head move) /= 'f')&&((head move) /= 'm'))) then do
                play l
            else if ((head move) == 'f') then do
                play (f (getPos ((ord (head(drop 1 move))-48),(ord (head(drop 2 move))-48) )) l) 
            else do  
                play (action (getPos ((ord (head(drop 1 move))-48),(ord (head(drop 2 move))-48) )) l)
--End of Drawing Functions
------------------------------------------------------------
main :: IO()
main = do 
    putStrLn "#    -> Unknown"
    putStrLn "!    -> Flagged"
    putStrLn "0    -> No Mines Around"
    putStrLn "1..8 -> 1..8 Mines Around"
    putStrLn "*    -> Mine"
    putStrLn "to Click some position just type << m position >>"
    putStrLn "to flag some position just type  << f  position >>"
    putStrLn "position >> xy"
    putStrLn "Example : m11 or f23"
    putStrLn ""
    
    n <- randomRIO (1,100)
    
    let mp2 = randomList mines n 1 r
    play (try mp2)


