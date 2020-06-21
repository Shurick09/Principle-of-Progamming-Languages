factorial 0 = 1
factorial n = n * factorial (n-1)

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 2) + fibonacci(n - 1)

len [] = 0
len (x:xs) = 1 + len xs

sumA [] = 0
sumA(x:xs) = x + sumA(xs)

maximumA [] = error ""
maximumA[x] = x
maximumA (x:xs) = max x (maximumA xs)
