module Problem3
(
    Claim(..),
    Rectangle(..),
    Point(..),
    intersection,
    area,
    contains,
    squaresInTwoOrMore,
    doesNotOverlapAnyOther
) where

data Claim = Claim Int Rectangle
    deriving (Show, Eq)
data Rectangle = Rect Int Int Int Int
    deriving (Show, Eq)

data Point = Point Int Int
    deriving (Show, Eq)

instance Read Claim where
    readsPrec _ input = readsClaim input

readsClaim s = [(Claim id rect, x) | ("#", a) <- lex s, (id, b) <- reads a, ("@", c) <- lex b, (rect, x) <- reads c]

instance Read Rectangle where
    readsPrec _ input = readsRect input

readsRect s = [(Rect left top width ((read . tail) height), f) | (left, a) <- reads s,
                                                     (",", b) <- lex a,
                                                     (top, c) <- reads b,
                                                     (":", d) <- lex c,
                                                     (width, e) <- reads d,
                                                     (height, f) <- lex e]



contains :: Point -> Rectangle -> Bool
contains (Point x y) (Rect left top width height) = x >= left && x < right && y >= top && y < bottom
    where right = left + width
          bottom = top + height

area :: Rectangle -> Int
area (Rect _ _ w h) = w*h

intersection :: Rectangle -> Rectangle -> Maybe Rectangle
intersection (Rect left1 top1 w1 h1) (Rect left2 top2 w2 h2) | (&&) (width > 0) (height > 0) = Just $ Rect left top width height
                                                             | otherwise = Nothing
    where right1 = left1 + w1
          right2 = left2 + w2
          bottom1 = top1 + h1
          bottom2 = top2 + h2
          left = max left1 left2
          right = min right1 right2
          width = right - left
          top = max top1 top2
          bottom = min bottom1 bottom2
          height = bottom - top

intersects :: Rectangle -> Rectangle -> Bool
intersects r1 r2 = case (intersection r1 r2) of
    Just _ -> True
    Nothing -> False

squaresInTwoOrMore :: [Claim] -> Int
squaresInTwoOrMore claims = length [ p | p <- points, length (filter (contains p) rectangles) > 1]
    where points = [ Point x y | x <- [0..1000], y <- [0..1000]]
          rectangles = map rect claims
          rect (Claim _ r) = r

doesNotOverlapAnyOther :: [Claim] -> [Claim]
doesNotOverlapAnyOther claims = filter (\c -> (not . doesClaimIntersectAnyOther) c) claims
    where doesClaimIntersectAnyOther claim = any id $ map (intersects (rect claim)) (rectangles (notMe claim))
          rectangles = map rect
          notMe (Claim id _) = filter (\(Claim otherId _) -> id /= otherId) claims
          rect (Claim _ r) = r
