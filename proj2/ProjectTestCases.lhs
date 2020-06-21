> module Main where
> 
> import Control.Exception (catch, ErrorCall(..), evaluate)
> import Project (RE(..), matchEmpty, firstMatches)
>
> a = Sym 'a'
> b = Sym 'b'
>
> tests =
>     [ test "matchEmpty a" (matchEmpty a) False
>     , test "matchEmpty a*b" (matchEmpty (Rep a :+: b)) False
>     , test "matchEmpty (ab)*" (matchEmpty (Rep (a :+: b))) True
>     , test "matchEmpty (a|e)+" (matchEmpty (Rep1 (a :|: Empty))) True
>     , testBy seteq "firstMatches a" (firstMatches a) "a"
>     , testBy seteq "firstMatches (a|b)*" (firstMatches (Rep (a :|: b))) "ab"
>     , testBy seteq "firstMatches 12" (firstMatches (Sym 1 :+: Sym 2)) [1]
>     , testBy seteq "firstMatches (1|e)2" (firstMatches ((Sym 1 :|: Empty) :+: Sym 2)) [1,2]
>     ]
> 
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
> seteq l1 l2 = all (`elem` l1) l2 && all (`elem` l2) l1
>
> runTests n f [] = 
>     putStrLn $ "Completed " ++ show n ++ " tests. " ++ show f ++ " failures."
> runTests n f (t:ts) = do
>     pass <- t
>     runTests (n+1) (if pass then f else f + 1) ts
>
> main = runTests 0 0 tests

