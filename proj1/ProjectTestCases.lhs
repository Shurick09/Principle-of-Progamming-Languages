> module Main where
>
> import Project
> import Control.Exception (catch, ErrorCall(..), evaluate)

This section defines some trees that will be used by the test cases.

> t1, t2 :: Tree Int
> t1 = Fork (Leaf 5) (Fork (Leaf 3) (Leaf 2))
> t2 = Fork (Fork (Leaf 0) (Leaf 2)) (Fork (Fork (Leaf (-1)) (Leaf 8)) (Leaf 13))
>
> b1 :: BST Char
> b1 = Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' Tip)
> b2, bbad :: BST Int
> b2 = Bin (Bin Tip (-1) (Bin Tip 1 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 (Bin Tip 7 Tip))
> bbad = Bin Tip 5 (Bin Tip 2 Tip)

Here are the test cases themselves. Each is defined using test or testBy,
and has a name, the value computed by your functions, and the expected value.

> tests =
>     [ test "cart [1,2,3] \"ab\""
>            (cart [1,2,3] "ab")
>            [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
>     , test "cart [] [5,6,7]" (cart "" [5,6,7]) []
>     , testBy approx "stddev [1,1,1,5]" (stddev [1,1,1,5]) (sqrt 3)
>     , testBy approx "stddev [12,1.5,0.66,1000]" 
>           (stddev [12,1.5,0.66,1000]) 430.9920414578441
>     , test "height t1" (height t1) 2
>     , test "height t2" (height t2) 3
>     , test "minLeaf t1" (minLeaf t1) 2
>     , test "minLeaf t2" (minLeaf t2) (-1)
>     , test "inorder t1" (inorder t1) [5,3,2]
>     , test "inorder t2" (inorder t2) [0,2,-1,8,13]
>     , test "contains 'b' b1" (contains 'b' b1) True
>     , test "contains 2 b2" (contains 2 b2) False
>     , test "contains 2 bbad" (contains 2 bbad) False
>     , test "contains 'd' (insert 'd' b1)" (contains 'd' (insert 'd' b1)) True
>     , test "contains 'a' (delete 'a' b1)" (contains 'a' (delete 'a' b1)) False
>     , test "delete 5 b2"
>           (let b3 = delete 5 b2
>            in and
>               [ contains (-1) b3
>               , contains 1 b3
>               , contains 4 b3
>               , not (contains 5 b3)
>               , contains 6 b3
>               , contains 7 b3
>               ])
>           True
>     ]

The remaining functions evaluate the test cases and report whether they
succeeded. They involve features of Haskell that we have not yet discussed.

> testBy :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> IO Bool
> testBy eq name got want = catch body (\(ErrorCall s) -> fail "ERROR" s)
>     where
>     body = do
>         ok <- evaluate (eq got want)
>         if ok
>             then do
>                 putStrLn $ "PASS : " ++ name
>                 return True
>             else fail "FAIL " (show got)
>     
>     fail msg txt = do
>         putStrLn $ msg ++ ": " ++ name
>         putStrLn $ "       wanted: " ++ show want
>         putStrLn $ "       got:    " ++ txt
>         return False
>
> test :: (Eq a, Show a) => String -> a -> a -> IO Bool
> test = testBy (==)
>
> approx x y = abs (x - y) / min x y <= 0.001
> 
> runTests n f [] = 
>     putStrLn $ "Completed " ++ show n ++ " tests. " ++ show f ++ " failures."
> runTests n f (t:ts) = do
>     pass <- t
>     runTests (n+1) (if pass then f else f + 1) ts
>
> main = runTests 0 0 tests
