module Project where

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Show, Eq, Ord)
data BST a = Tip | Bin (BST a) a (BST a) deriving (Show, Eq, Ord)

--Returns the cartesian product of the lists [a] and [b]
cart :: [a] -> [b] -> [(a, b)]
cart xs ys = [(x,y) | x <- xs , y <- ys]

--Returns the standard deviation of [Double]
stddev :: [Double] -> Double
stddev xs = sqrt (sum (tempList (xs, avg xs)) / realToFrac (length xs))

--Creates a list of how far each element is from the average squared
tempList :: ([Double],Double) -> [Double]
tempList ([],y) = []
tempList ((x:xs),average) = (x - average)^2 : tempList (xs, average)

--Returns the average of the list [Double]
avg :: [Double] -> Double
avg xs =  sum (xs) / realToFrac (length (xs))

--Returns the height of a Tree
height :: Tree a -> Int
height (Leaf _) = 0
height (Fork (l)(r)) = 1 + max (height l) (height r)

--Returns the value of the smallest tree in the list
minLeaf :: Ord a => Tree a -> a
minLeaf (Leaf a) = a
minLeaf (Fork (l)(r)) = min (minLeaf l)(minLeaf r) 

--Returns a list of the elements after an inorder (left to right) traversal of the tree
inorder :: Tree a -> [a]
inorder (Leaf a) = [a]
inorder (Fork (l)(r)) = inorder (l) ++ inorder(r)

--Returns true or false depending on whether the BST contains a value
contains :: Ord a => a -> BST a -> Bool
contains _ Tip = False
contains y (Bin (l) x (r))
    | y == x = True
    | y > x = contains y r
    | y < x = contains y l

--Inserts a value into the BST
insert :: Ord a => a -> BST a -> BST a
insert y Tip = Bin (Tip) y (Tip)
insert y (Bin (l) x (r))
    | y == x = Bin (l) x (r)
    | y > x = Bin l x (insert y r)
    | y < x = Bin (insert y l) x r

--Returns a BST with a deleted value
delete :: (Ord a) => a -> BST a -> BST a
delete _ Tip = Tip
delete y (Bin (l) x (r))   
	| y == x = deleteSubTree (Bin (l) x (r))
	| y > x = Bin l x (delete y r)
        | y < x = Bin (delete y r) x r

--When the value is found it removes it from the BST
deleteSubTree :: (Ord a) => BST a -> BST a 
deleteSubTree (Bin Tip x (r)) = r
deleteSubTree (Bin (l) x Tip) = l
deleteSubTree (Bin l x r) = (Bin (l) (swapToDelete r) (r)) 

--Called when the value to be deleted is not a Tip
--Finds the left most value and swaps that value with the value to be deleted
--Then deletes the value which is now a Tip
swapToDelete :: (Ord a) => BST a -> a
swapToDelete (Bin Tip x _) = x
swapToDelete (Bin l _ _) = swapToDelete l


