data Poly a = P [a] deriving (Show, Eq)

trim :: (Num a, Eq a) => [a] -> [a]
trim[] = []
trim(0:xs) = case trim xs of
    []->[]
    ys -> 0:ys
trim(x:xs) = x : trim xs



degree :: Poly a -> Int
degree (P[]) = 0
degree (P[a]) = 0
degree (P(x:xs)) = 1 + degree (P(xs))



scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale _ (P[]) = (P[])
scale 0 _ = (P[])
scale a (P(x:xs)) = P(map (a *) (x:xs))



($$) :: (Num a, Eq a) => Poly a -> a -> a
P(x:[]) $$ a = x
P(x:xs) $$ a = x + a * P(xs) $$ a



addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
addPoly (P a) (P b) = P(trim(add a b))

add :: (Num a, Eq a) => [a] -> [a] -> [a] 
add [] b = b
add a [] = a
add (a:as) (b:bs) = (a+b) : add as bs



multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly (P a) (P b) = P(trim(mult a b))

mult :: (Num a, Eq a) => [a] -> [a] -> [a]
mult [] b = []
mult a [] = []
mult a (b:bs) = add (map (b *) a) (0 : mult a bs)

instance (Num a, Eq a) => Num (Poly a) where
    (+) = addPoly
    negate = scale (-1)
    (*) = multPoly
    fromInteger 0 = P []
    fromInteger n = P [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

x :: (Num a) => Poly a
x = P [0,1]


y :: (Num a) => Poly (Poly a)
y = P [P [0,1]]
