 
data Term = Var Int | Lam Term | App Term Term deriving (Eq,Show,Read)
 
encode (Var x) 
 | x==0 = [0]
 |otherwise = [1]++[0]
encode (App x y) =[0,0]++(encode x) ++ (encode y)
encode (Lam x) = [0,1]++ (encode x)

{-
-- Zip function
zip_::[a]->[b]->[(a,b)]
zip_ x y = [(t,t1) | t<-x, t1<-y ]
zip_ [] [] = []

-}

--which


worm = 1:2:3:worm
eel = [1,2,3,1,2,3,1,2,3]
snake = 3:2:1:snake
whale = [1..100]


which x = check_ (take 100 x) where 
     check_ (3:2:1:_) = "snake"
     check_ (1:2:3:4:_) = "whale"

parenthesis::(Num a,Ord a)=>[a]->[a]->Int->[a]
parenthesis a [0] _=[0]
parenthesis a [1] _ =[1]
parenthesis a [] _ =[]
parenthesis a (x:y:xs) i 
 |(x==0)&& ((reverse(a))!!i == 1) = [0] ++ parenthesis a (y:xs) (i+1)
 |(x==0)&& ((reverse(a))!!i == 0) = [0]++ parenthesis a (y:xs) (i+1) ++ [1]
 |((x==1) && (y==1)) = [1] ++ parenthesis a xs (i+1)
 |otherwise = [1] ++ parenthesis a (y:xs) (i+1)


put x
 | (x!!0 /=1) = print (parenthesis x x 0)
 | otherwise = print "error"
-- Replacing hello with good bye in string.

-- gets the starting index 'h' 
re x y i
 | i < (length x-3) =  check x (map (x!!) [i..(i+4)]) i
 | otherwise = "Not present :p"


-- construct new  string inserting "Good - Bye" at position 'i' in x
insert::String->Int->String
insert x i = map (x!!) [0..(i-1)] ++ "Good-Bye" ++ map (x!!) [(i+5).. (length x -1)]

replace x = re x "" 0 

-- checks if the string is "Hello"
check x y i
 | y=="hello" = insert x i
 | otherwise = re x "" (i+1)

append_foldl::[a]->[a]->[a]
append_foldl [] ys = ys
append_foldl xs ys = foldr (:) ys xs

insert_z [x]=[x]
insert_z (x:xs) = [x]++[0]++(insert_z xs) 

seperate []=([],[])
seperate x = ([z|z<-x, ( (mod z 2)/=0)],[z|z<-x,( (mod z 2)  ==0)])