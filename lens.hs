-- Concrete lens definition
data Lens s t a b = Lens {
    view :: s -> a,
    update :: (b, s) -> t
}

-- Composition operator
(|.|) :: Lens s t a b -> Lens a b c d -> Lens s t c d
(Lens v1 u1) |.| (Lens v2 u2) = Lens v u where
    v = v2 . v1
    u (d, s) = u1 (u2 (d, v1 s), s)

-- Projection on tuples
p1 :: Lens (a, c) (b, c) a b
p1 = Lens view update where
    view = fst
    update (b, (_, c)) = (b, c)

p2 :: Lens (a, b) (a, c) b c
p2 = Lens view update where
    view = snd
    update (c, (a, _)) = (a, c)

--How to use it
--thrice = p1 |.| p1 |.| p1
--nest = (((1, 'a'), 2.0), True)
--view thrice nest == 1
--update thrice ("hi", nest) == ((("hi", 'a'), 2.0), True)

-- Control the sign of an integer
sign :: Lens Integer Integer Bool Bool
sign = Lens view update where
    view = (>= 0)
    update (makePositive, n) = if makePositive then abs n else -(abs n) 

-- view sign 4 == True
-- update sign (False, 4) == -4

