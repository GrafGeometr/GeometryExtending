{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Functions where


import Data.Complex

import RandomGen
import Types
import qualified Data.Map as Map
import Debug.Trace (trace)



rndPoint :: Rand Point
rndPoint = do
    x <- random (-100) 100
    y <- random (-100) 100
    return $ x :+ y


i :: Point
i = 0 :+ 1

re :: Point -> Double
re (x :+ _) = x

im :: Point -> Double
im (_ :+ y) = y

arg :: Point -> Double
arg (x :+ y) = atan2 y x

conj :: Point -> Point
conj (x :+ y) = x :+ (-y)

norm :: Point -> Double
norm (x :+ y) = x * x + y * y

infixl 6 .+, +., .-, -.
infixl 7 .*, *., ./, /.

(.*) :: Double -> Point -> Point
a .* b = (a :+ 0) * b

(*.) :: Point -> Double -> Point
a *. b = a * (b :+ 0)

(.+) :: Double -> Point -> Point
a .+ b = (a :+ 0) + b

(+.) :: Point -> Double -> Point
a +. b = a + (b :+ 0)

(.-) :: Double -> Point -> Point
a .- b = (a :+ 0) - b

(-.) :: Point -> Double -> Point
a -. b = a - (b :+ 0)

(./) :: Double -> Point -> Point
a ./ b = (a :+ 0) / b

(/.) :: Point -> Double -> Point
a /. b = a / (b :+ 0)


intersect :: Line -> Line -> Point
intersect l1 l2 = (coef l1 *. free l2 - free l1 .* coef l2) / ((conj $ coef l1) * coef l2 - coef l1 * (conj $ coef l2)) 

line :: Point -> Point -> Line
line a b = Line ((a - b) * i) $ re (conj a * b * i) * 2

midline :: Point -> Point -> Line
midline a b = Line (a - b) (norm b - norm a)

symmetry :: Point -> Line -> Point
symmetry a l = -(conj a * coef l +. free l) / (conj $ coef l)

project :: Point -> Line -> Point
project a l = (a - (conj a * coef l +. free l) / (conj $ coef l)) / 2


{-
line.function("parallel", point, line) { a, l ->
    Line(l.coef, -(a.conj() * l.coef).re * 2)
},
-}

parallel :: Point -> Line -> Line
parallel a l = Line (coef l) $ - re (conj a * coef l) * 2



rndPointOnLine :: Line -> Rand Point
rndPointOnLine l = do
    p <- rndPoint
    return $ project p l


circle :: Point -> Point -> Circle
circle c p = Circle c $ norm (p - c)


root :: Point -> Point -> Point -> Point
root r a b = - b / a - r


cproject :: Point -> Circle -> Point
cproject p c =
    let v = p - center c
        n = v *. sqrt (radiusSqr c / norm v)
    in  center c + n


rndPointOnCircle :: Circle -> Rand Point
rndPointOnCircle c = do
    p <- rndPoint
    return $ cproject p c


cintersect :: Point -> Line -> Circle -> Point
cintersect p l c = root
    p
    (conj $ coef l)
    (free l .+ conj (center c) * coef l - center c * conj (coef l))


ccintersect :: Point -> Circle -> Circle -> Point
ccintersect p c1 c2 =
    let it = conj (center c2 - center c1)
    in  root p it ((radiusSqr c2 - radiusSqr c1) .- (center c2 + center c1) * it)


circumcenter :: Point -> Point -> Point -> Point
circumcenter a b c = ((b - c) *. norm a + (c - a) *. norm b + (a - b) *. norm c) /
            ((b - c) * conj a + (c - a) * conj b + (a - b) * conj c)

circumcircle :: Point -> Point -> Point -> Circle
circumcircle a b c = circle (circumcenter a b c) a


data Angle = Angle {vertex :: Point, from :: Double, to :: Double}

angle :: Point -> Point -> Point -> Angle
angle a b c = Angle b (arg (a - b)) $ arg (c - b)



bisector :: Point -> Point -> Point -> Line
bisector a b c = 
    let an = angle a b c
        it = exp $ ((to an + from an + pi) * 0.5) .* i
    in  Line it $ re (-(vertex an * conj it)) * 2

exbisector :: Point -> Point -> Point -> Line
exbisector a b c =
    let an = angle a b c
        it = exp $ ((to an + from an) * 0.5) .* i
    in  Line it $ re (-(vertex an * conj it)) * 2        

excenter :: Point -> Point -> Point -> Point
excenter a b c =
    let sa = sqrt (b - c)
        sb = sqrt (c - a)
        sc = sqrt (a - b)
    in  (a * sa * conj sc - c * conj sa * sc + sa * sc *. norm (a - c) /. norm sb) / (sa * conj sc - conj sa * sc)

incenter :: Point -> Point -> Point -> Point
incenter a b c =
    let sa = sqrt (b - c)
        sb = sqrt (c - a)
        sc = sqrt (a - b)
    in  (a * sa * conj sc - c * conj sa * sc - sa * sc *. norm (a - c) /. norm sb) / (sa * conj sc - conj sa * sc)

orthocenter :: Point -> Point -> Point -> Point
orthocenter a b c = ((b - a) *. norm c + (c - b) *. norm a + (a - c) *. norm b + (a * a - b * b) * conj c + (b * b - c * c) * conj a + (c * c - a * a) * conj b) / 
            ((b - c) * conj a + (c - a) * conj b + (a - b) * conj c)

functions :: Map.Map String ([Shape] -> [Rand Shape])
functions = Map.fromList 
    [  (
        "Centroid", mkBuilder $ \a b c -> 
        [ rndPointOnLine $ line a $ (b + c) / 2
        , rndPointOnLine $ line b $ (a + c) / 2
        , rndPointOnLine $ line c $ (a + b) / 2
        , pure $ (a + b + c) / 3
        ]
    ), (
        "Circumcenter", mkBuilder $ \a b c -> 
        [ rndPointOnLine $ midline a b
        , rndPointOnLine $ midline b c
        , rndPointOnLine $ midline a c
        , return $ circumcenter a b c
        ]
    ), (
        "Excenter", mkBuilder $ \b a c ->
        [ rndPointOnLine $ exbisector a b c
        , rndPointOnLine $ bisector a c b
        , rndPointOnLine $ bisector b a c
        , return $ excenter a b c
        ]
    ), (
        "Incenter", mkBuilder $ \a b c ->
        [ rndPointOnLine $ bisector a b c
        , rndPointOnLine $ bisector b a c
        , rndPointOnLine $ bisector a c b
        , rndPoint, return $ incenter a b c
        ]
    ), (
        "IntersectionOfLinesFromPoints", mkBuilder $ \a b c d ->
        let l1 = line a b
            l2 = line c d
        in  [ rndPointOnLine l1
            , rndPointOnLine l2
            , return $ intersect l1 l2
            ]
    ), (
        "IsoscelesTrapezoidPoint", mkBuilder $ \a b c ->
        let m  = midline c b
        in  [ rndPointOnLine $ parallel a (line b c)
            , return $ symmetry a m
            ]
    ), (
        "Midpoint", mkBuilder $ \a b -> 
        [ rndPointOnLine $ line a b
        , return $ (a + b) / 2
        ]
    ), (
        "MidpointOfArc", mkBuilder $ \a b c -> 
        [ rndPointOnCircle $ circumcircle a b c
        , return $ cintersect a (exbisector c a b) (circumcircle a b c)
        ]
    ), (
        "MidpointOfOppositeArc", mkBuilder $ \a b c ->
        [ rndPointOnCircle $ circumcircle a b c
        , return $ cintersect a (bisector c a b) (circumcircle a b c)
        ]
    ), (
        "OppositePointOnCircumcircle", mkBuilder $ \a b c ->
        [ rndPointOnCircle $ circumcircle a b c
        , return $ 2 * circumcenter a b c - a
        ]
    ), (
        "Orthocenter", mkBuilder $ \a b c -> 
        [ rndPointOnLine $ line a $ project a $ line b c
        , rndPointOnLine $ line b $ project b $ line a c
        , rndPointOnLine $ line c $ project c $ line a b
        , return $ orthocenter a b c
        ]
    ), (
        "ParallelogramPoint", mkBuilder $ \a b c ->
        [ rndPointOnLine $ line a $ (b + c) / 2
        , rndPointOnLine $ parallel b (line a c)
        , rndPointOnLine $ parallel c (line a b)
        , return $ b + c - a
        ]
    ), (
        "PerpendicularProjectionOnLineFromPoints", mkBuilder $ \a b c ->
        let l = line b c
        in  [ rndPointOnLine l
            , rndPointOnLine $ line a $ project a l
            , return $ (a - (conj a * coef l +. free l) / (conj $ coef l)) / 2
            ]
    ), (
        "PointReflection", mkBuilder $ \a b -> 
        [ rndPointOnLine $ line a b
        , return $ 2 * b - a
        ]
    ), (
        "ReflectionInLineFromPoints", mkBuilder $ \a b c -> 
        [ rndPointOnLine $ line a $ project a $ line b c
        , return $ symmetry a (line b c)
        ]
    ), (
        "SecondIntersectionOfCircleAndLineFromPoints", mkBuilder $ \a b c d -> 
        [ rndPointOnCircle $ circumcircle a c d
        , rndPointOnLine $ line a b
        , return $ cintersect a (line a b) (circumcircle a c d)
        ]
    ), (
        "SecondIntersectionOfTwoCircumcircles", mkBuilder $ \a b c d e -> 
        [ rndPointOnCircle $ circumcircle a b c
        , rndPointOnCircle $ circumcircle a d e
        , return $ ccintersect a (circumcircle a b c) (circumcircle a d e)
        ]
    )
    ]


eps :: Double
eps = 0.01

checkEq :: Double -> Double -> Bool
checkEq x y = max (abs $ x / y) (abs $ y / x) < 1 + eps

checkReal :: Point -> Bool
checkReal x = abs (im x) < eps || abs (im x) < eps * abs (re x)

checkImagine :: Point -> Bool
checkImagine x = abs (re x) < eps || abs (re x) < eps * abs (im x)

checkEqPts :: Point -> Point -> Bool
checkEqPts a b = checkEq (re a) (re b) && checkEq (im a) (im b)


factCheckers :: Map.Map String ([Shape] -> Bool)
factCheckers = Map.fromList
    [  (
        "CollinearPoints", mkChecker $ \a b c -> checkReal $ (b - a) * conj (c - a)
    ), (
        "ConcyclicPoints", mkChecker $ \a b c d -> checkReal $ (a - b) * (d - c) * conj (b - c) * conj (a - d)
    ), (
        "ConcurrentLines", mkChecker $ \l1 l2 l3 ->
        let t12 = intersect l1 l2
            t13 = intersect l1 l3
        in  checkEqPts t12 t13
    ), (
        "EqualLineSegments", mkChecker $ \a b c d -> checkEq (norm $ b - a) (norm $ d - c)
    ), (
        "LineTangentToCircle", mkChecker $ \l cs ->
        let p = center cs `project` l
        in  checkEq (norm $ p - center cs) (radiusSqr cs)
    ), (
        "TangentCircles", mkChecker $ \c1 c2 ->
        let r1sq = radiusSqr c1
            r2sq = radiusSqr c2
            ds = norm $ center c1 - center c2
        in  checkEq (sqrt r1sq + sqrt r2sq) ds
    ), (
        "ParallelLines", mkChecker $ \l1 l2 -> checkReal $ coef l1 / coef l2
    ), (
        "PerpendicularLines", mkChecker$  \l1 l2 -> checkImagine $ coef l1 / coef l2
    )
    ]

rndPointCmd :: String -> Command
rndPointCmd s = Command s [] (const [toShape <$> rndPoint])


constPointCmd :: String -> Point -> Command
constPointCmd s p = Command s [] (const [pure $ toShape p])


anyPolygon :: [String] -> [Command]
anyPolygon = fmap $ \n -> Command n [] (const [toShape <$> rndPoint])


rightTriangle :: [String] -> [Command]
rightTriangle [a, b, c] = 
    [ constPointCmd a 0
    , constPointCmd b (50.*i)
    , Command c [] (const $ [toShape <$> (:+0) <$> random 10 100])
    ]

cyclicQuadrilateral :: [String] -> [Command]
cyclicQuadrilateral [a, b, c, d] = 
    [ rndPointCmd a
    , rndPointCmd b
    , rndPointCmd c
    , Command d [a, b, c] $ mkBuilder $ \x y z -> [rndPoint, rndPointOnCircle $ circumcircle x y z]
    ]

initials :: Map.Map String ([String] -> [Command])
initials = Map.fromList
    [ ("Triangle", anyPolygon)
    , ("RightTriangle", rightTriangle)
    , ("LineSegment", anyPolygon)
    , ("Quadrilateral", anyPolygon)
    , ("CyclicQuadrilateral", cyclicQuadrilateral)
    ]