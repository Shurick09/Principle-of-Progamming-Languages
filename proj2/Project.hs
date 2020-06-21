module Project where

data RE a            -- regular expressions over an alphabet defined by 'a'
    = Empty          -- empty regular expression
    | Sym a          -- match the given symbol
    | RE a :+: RE a  -- concatenation of two regular expressions
    | RE a :|: RE a  -- choice between two regular expressions
    | Rep (RE a)     -- zero or more repetitions of a regular expression
    | Rep1 (RE a)    -- one or more repetitions of a regular expression
    deriving (Show)

--This function returns true if the regular expression matches an empty string.
matchEmpty :: RE a -> Bool
matchEmpty Empty = True
matchEmpty (Sym a) = False
matchEmpty (l :+: r) = matchEmpty l && matchEmpty r
matchEmpty (l :|: r) = matchEmpty l || matchEmpty r
matchEmpty (Rep x) = True
matchEmpty (Rep1 x) = matchEmpty x

--Given a regular expression, return a list of all symbols that occur first in some string in the language described by r.
firstMatches :: RE a -> [a]
firstMatches a = firstMatchesHelper (a,[])

--Helper function for firstMatches that keeps track of the list of all symbols that could occur first
firstMatchesHelper :: (RE a, [a]) ->  [a]
firstMatchesHelper ((Empty),[]) = []
firstMatchesHelper ((Empty), [xs]) = [xs]
firstMatchesHelper ((Sym a), []) = a : []
firstMatchesHelper ((Sym a), [xs]) = a : [xs]
firstMatchesHelper ((l :+: r), [])  
    |matchEmpty l == True = firstMatchesHelper(l, []) ++ firstMatchesHelper (r, [])
    |otherwise = firstMatchesHelper(l,[])
firstMatchesHelper ((l :+: r), [xs]) 
    |matchEmpty l == True = firstMatchesHelper(l, [xs]) ++ firstMatchesHelper (r, [xs])
    |otherwise = firstMatchesHelper(l,[xs])
firstMatchesHelper ((l :|: r), []) =  firstMatchesHelper (l, []) ++ firstMatchesHelper (r, [])
firstMatchesHelper ((l :|: r), [xs]) =  firstMatchesHelper (l, [xs]) ++ firstMatchesHelper (r, [xs]) 
firstMatchesHelper ((Rep x), []) = firstMatchesHelper (x,[])
firstMatchesHelper ((Rep x), [xs]) =  firstMatchesHelper (x, [xs])
firstMatchesHelper ((Rep1 x),[]) = firstMatchesHelper (x,[]) 
firstMatchesHelper ((Rep1 x),[xs]) =  firstMatchesHelper (x, [xs])


