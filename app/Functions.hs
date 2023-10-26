{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Functions where

import RandomGen
import Types
import qualified Data.Map as Map



rndPoint :: Rand Point
rndPoint = (:+) <$> random <*> random

i :: Point
i = 0 :+ 1

re :: Point -> Fin
re (x :+ _) = x

im :: Point -> Fin
im (_ :+ y) = y

conj :: Point -> Point
conj (x :+ y) = x :+ (-y)

norm :: Point -> Fin
norm (x :+ y) = x * x + y * y

infixl 6 .+, +., .-, -.
infixl 7 .*, *., ./, /.

(.*) :: Fin -> Point -> Point
a .* b = (a :+ 0) * b

(*.) :: Point -> Fin -> Point
a *. b = a * (b :+ 0)

(.+) :: Fin -> Point -> Point
a .+ b = (a :+ 0) + b

(+.) :: Point -> Fin -> Point
a +. b = a + (b :+ 0)

(.-) :: Fin -> Point -> Point
a .- b = (a :+ 0) - b

(-.) :: Point -> Fin -> Point
a -. b = a - (b :+ 0)

(./) :: Fin -> Point -> Point
a ./ b = (a :+ 0) / b

(/.) :: Point -> Fin -> Point
a /. b = a / (b :+ 0)


intersect :: Line -> Line -> Point
intersect l1 l2 = (coef l1 *. free l2 - free l1 .* coef l2) / (conj (coef l1) * coef l2 - coef l1 * conj (coef l2))

line :: Point -> Point -> Line
line a b = Line ((a - b) * i) $ re (conj a * b * i) * 2

midline :: Point -> Point -> Line
midline a b = Line (a - b) (norm b - norm a)

symmetry :: Point -> Line -> Point
symmetry a l = -(conj a * coef l +. free l) / conj (coef l)

project :: Point -> Line -> Point
project a l = (a - (conj a * coef l +. free l) / conj (coef l)) / 2


{-
line.function("parallel", point, line) { a, l ->
    Line(l.coef, -(a.conj() * l.coef).re * 2)
},
-}

parallel :: Point -> Line -> Line
parallel a l = Line (coef l) $ - re (conj a * coef l) * 2

{-
line.function("perpendicular", point, line) { a, l ->
    Line(l.coef * Complex.I, -(a.conj() * l.coef * Complex.I).re * 2)
},
-}

perpendicular :: Point -> Line -> Line
perpendicular a l = Line (coef l * i) (- (re (conj a * coef l * i) * 2))

rndPointOnLine :: Line -> Rand Point
rndPointOnLine l = do
    p <- rndPoint
    return $ project p l


circle :: Point -> Point -> Circle
circle c p = Circle c $ norm (p - c)


root :: Point -> Point -> Point -> Point
root r a b = - b / a - r


cproject :: Point -> Circle -> Rand Point
cproject p c = let v = p - center c in
    (\s -> center c + v *. s) <$> dsqrt (radiusSqr c / norm v)


rndPointOnCircle :: Circle -> Rand Point
rndPointOnCircle c = do
    p <- rndPoint
    cproject p c

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

bisector :: Point -> Point -> Point -> Rand Line
bisector a b c = (\s -> midline a (b + (c - b) *. s)) <$> dsqrt (norm (a - b) / norm (c - b))

incenter :: Point -> Point -> Point -> Rand Point
incenter a b c = intersect <$> bisector a b c <*> bisector a c b

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
        , return $ (a + b + c) / 3
        ]
    ), (
        "CircleWithCenterThroughPoint",
        mkBuilder $ \a b ->
        [ circle a <$> rndPoint
        , (`circle` a) <$> rndPoint
        , return $ circle a b
        ]
    ), (
        "CircleWithDiameter",
        mkBuilder $ \a b ->
        [ (`circle` a) <$> rndPointOnLine (line a b)
        , (`circle` b) <$> rndPointOnLine (line b a)
        , circle ((a + b) / 2) <$> rndPoint
        , (`circle` a) <$> rndPointOnLine (midline a b)
        , return $ circle ((a + b) / 2) b
        ]
    ), (
        "Circumcenter", mkBuilder $ \a b c ->
        [ rndPointOnLine $ midline a b
        , rndPointOnLine $ midline b c
        , rndPointOnLine $ midline a c
        , return $ circumcenter a b c
        ]
    ), (
        "Circumcircle",
        mkBuilder $ \a b c ->
        [ circle (circumcenter a b c) <$> rndPoint
        , (`circle` a) <$> rndPointOnLine (midline a b)
        , (`circle` b) <$> rndPointOnLine (midline b c)
        , (`circle` c) <$> rndPointOnLine (midline a c)
        , return $ circle (circumcenter a b c) a
        ]
    ), (
        "Excenter", mkBuilder $ \b a c ->
        [ bisector a b c >>= rndPointOnLine
        , bisector a c b >>= rndPointOnLine
        , bisector b a c >>= rndPointOnLine
        , incenter a b c
        ]
    ), (
        "Excircle",
        mkBuilder $ \b a c ->
        [ do
            p <- bisector a b c >>= rndPointOnLine
            return $ circle p (project p $ line b c)
        , do
            p <- bisector a c b >>= rndPointOnLine
            return $ circle p (project p $ line a c)
        , do
            p <- bisector b a c >>= rndPointOnLine
            return $ circle p (project p $ line b a)
        , (\i_b -> circle i_b (project i_b $ line a c)) <$> incenter a b c
        ]
    ), (
        "ExternalAngleBisector",
        mkBuilder $ \a b c ->
        [ line a <$> rndPoint
        , bisector b a c] :: [Rand Line]
    ), (
        "Incenter", mkBuilder $ \a b c ->
        [ bisector a b c >>= rndPointOnLine
        , bisector b a c >>= rndPointOnLine
        , bisector a c b >>= rndPointOnLine
        , incenter a b c
        ]
    ), (
        "Incircle",
        mkBuilder $ \a b c ->
        [ do
            p <- bisector a b c >>= rndPointOnLine
            return $ circle p (project p $ line a b)
        , do
            p <- bisector b a c >>= rndPointOnLine
            return $ circle p (project p $ line b a)
        , do
            p <- bisector a c b >>= rndPointOnLine
            return $ circle p (project p $ line a c)
        , (\inc -> circle inc (project inc $ line a b)) <$> incenter a b c
        ]
    ), (
        "InternalAngleBisector",
        mkBuilder $ \a b c ->
        [ line a <$> rndPoint
        , bisector b a c
        ]
    ), (
        "IntersectionOfLineAndLineFromPoints",
        mkBuilder $ \l a b ->
        [ rndPointOnLine l
        , rndPointOnLine $ line a b
        , return $ intersect l (line a b)
        ]
    ), (
        "IntersectionOfLines",
        mkBuilder $ \l1 l2 ->
        [ rndPointOnLine l1
        , rndPointOnLine l2
        , return $ intersect l1 l2
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
        "LineFromPoints",
        mkBuilder $ \a b ->
        [ line a <$> rndPoint
        , line b <$> rndPoint
        , return $ line a b
        ]
    ), (
        "LineThroughCircumcenter",
        mkBuilder $ \a b c ->
        [ line a <$> rndPoint
        , line (circumcenter a b c) <$> rndPoint
        , return $ line (circumcenter a b c) a
        ]
    ), (
        "Median",
        mkBuilder $ \a b c ->
        [ line a <$> rndPoint
        , line ((b + c) / 2) <$> rndPoint
        , return $ line a ((b + c) / 2)
        ]
    ), (
        "Midline", mkBuilder $ \a b c ->
        [ line ((a + b) / 2) <$> rndPoint
        , line ((a + c) / 2) <$> rndPoint
        , (`parallel` line b c) <$> rndPoint
        , return $ line ((a + b) / 2) ((a + c) / 2) ] :: [Rand Line]
    ), (
        "Midpoint", mkBuilder $ \a b ->
        [ rndPointOnLine $ line a b
        , return $ (a + b) / 2
        ]
    ), (
        "MidpointOfArc", mkBuilder $ \a b c ->
        [ bisector c a b >>= rndPointOnLine
        , rndPointOnCircle $ circumcircle a b c
        , cintersect a <$> bisector c a b <*> pure (circumcircle a b c)
        ]
    ), (
        "MidpointOfOppositeArc", mkBuilder $ \a b c ->
        [ bisector c a b >>= rndPointOnLine
        , rndPointOnCircle $ circumcircle a b c
        , cintersect a <$> bisector c a b <*> pure (circumcircle a b c)
        ]
    ), (
        "NinePointCircle",
        mkBuilder $ \a b c ->
        [ return $ circumcircle ((a + b) / 2) ((b + c) / 2) ((c + a) / 2) ] :: [Rand Circle]
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
        "ParallelLine",
        mkBuilder $ \a l ->
        [ line a <$> rndPoint
        , (`parallel` l) <$> rndPoint
        , return $ parallel a l
        ]
    ), (
        "ParallelLineToLineFromPoints",
        mkBuilder $ \a b c ->
        [ line a <$> rndPoint
        , (`parallel` line b c) <$> rndPoint
        , return $ parallel a (line b c)
        ]
    ), (
        "ParallelogramPoint", mkBuilder $ \a b c ->
        [ rndPointOnLine $ line a $ (b + c) / 2
        , rndPointOnLine $ parallel b (line a c)
        , rndPointOnLine $ parallel c (line a b)
        , return $ b + c - a
        ]
    ), (
        "PerpendicularBisector",
        mkBuilder $ \a b ->
        [ line ((a + b) / 2) <$> rndPoint
        , (`perpendicular` line a b) <$> rndPoint
        , return $ perpendicular ((a + b) / 2) (line a b)
        ]
    ), (
        "PerpendicularLineAtPointOfLine",
        mkBuilder $ \a b ->
        [ (`perpendicular` line a b) <$> rndPoint
        , line a <$> rndPoint
        , return $ perpendicular a (line a b)
        ]
    ), (
        "PerpendicularLineToLineFromPoints",
        mkBuilder $ \a b c ->
        [ (`perpendicular` line b c) <$> rndPoint
        , line a <$> rndPoint
        , return $ perpendicular a (line b c)
        ]
    ), (
        "PerpendicularProjection",
        mkBuilder $ \a l ->
        [ rndPointOnLine l
        , rndPointOnLine $ line a $ project a l
        , return $ (a - (conj a * coef l +. free l) / conj (coef l)) / 2]
    ), (
        "PerpendicularProjectionOnLineFromPoints", mkBuilder $ \a b c ->
        let l = line b c
        in  [ rndPointOnLine l
            , rndPointOnLine $ line a $ project a l
            , return $ (a - (conj a * coef l +. free l) / conj (coef l)) / 2
            ]
    ), (
        "PointReflection", mkBuilder $ \a b ->
        [ rndPointOnLine $ line a b
        , return $ 2 * b - a
        ]
    ), (
        "ReflectionInLine",
        mkBuilder $ \a l ->
        [ return $ 2 * project a l - a ] :: [Rand Point]
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
    ), (
        "TangentLine",
        mkBuilder $ \a b c ->
        [ return $ perpendicular a $ line a (circumcenter a b c) ] :: [Rand Line]
    )
    ]
checkReal :: Point -> Bool
checkReal x = im x == 0

checkImagine :: Point -> Bool
checkImagine x = re x == 0


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
        in t12 == t13
    ), (
        "EqualLineSegments", mkChecker $ \a b c d -> norm (b - a) == norm (d - c)
    ), (
        "LineTangentToCircle", mkChecker $ \cs l ->
        let p = center cs `project` l
        in norm (p - center cs) == radiusSqr cs
    ), (
        "TangentCircles", mkChecker $ \c1 c2 ->
        let r1sq = radiusSqr c1
            r2sq = radiusSqr c2
            ds = norm $ center c1 - center c2
        in r1sq * r2sq == (ds - r1sq - r2sq) ^ (2 :: Int)
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
    , Command c [] (const $ (toShape <$>) <$> [rndPoint, (:+0) <$> random])
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