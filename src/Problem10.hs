module Problem10
(
    Point,
    Position,
    Velocity,
    nextStep,
    boundedRectSize,
    testInput,
    input,
    debug,
    debugPoints
)
where

type Position = (Int, Int)
type Velocity = (Int, Int)
data Point = Point { position :: Position, velocity :: Velocity }
    deriving Show

nextStep :: Point -> Point
nextStep (Point pos velocity) = Point (add pos velocity) velocity

add :: Position -> Velocity -> Position
add (x, y) (vx, vy) = (x + vx, y + vy)

debugPoints :: [Point] -> String
debugPoints points = debug $ map position points

debug :: [Position] -> String
debug positions = [ c | y <- [y..y+h], c <- (generateRow y) ++ "\n" ]
    where generateRow y = [ label (x,y) | x <- [x..x+w] ]
          (Rect (x,y) w h) = boundedRect positions
          label l = if elem l positions then '#' else '.'

data Rect = Rect { topLeft :: Position, width :: Int, height :: Int }
    deriving (Show, Eq)

boundedRectSize :: [Point] -> Int
boundedRectSize points = (x+width+1) * (y+height+1)
    where (Rect (x,y) width height) = boundedRect $ map position points

boundedRect :: [Position] -> Rect
boundedRect locations = Rect (topLeftX, topLeftY) width height
    where topLeftX = minimum $ map fst locations
          topLeftY = minimum $ map snd locations
          bottomRightX = maximum $ map fst locations
          bottomRightY = maximum $ map snd locations
          width = bottomRightX - topLeftX
          height = bottomRightY - topLeftY


testInput :: [Point]
testInput = [
    Point ( 9,  1) ( 0,  2),
    Point ( 7,  0) (-1,  0),
    Point ( 3, -2) (-1,  1),
    Point ( 6, 10) (-2, -1),
    Point ( 2, -4) ( 2,  2),
    Point (-6, 10) ( 2, -2),
    Point ( 1,  8) ( 1, -1),
    Point ( 1,  7) ( 1,  0),
    Point (-3, 11) ( 1, -2),
    Point ( 7,  6) (-1, -1),
    Point (-2,  3) ( 1,  0),
    Point (-4,  3) ( 2,  0),
    Point (10, -3) (-1,  1),
    Point ( 5, 11) ( 1, -2),
    Point ( 4,  7) ( 0, -1),
    Point ( 8, -2) ( 0,  1),
    Point (15,  0) (-2,  0),
    Point ( 1,  6) ( 1,  0),
    Point ( 8,  9) ( 0, -1),
    Point ( 3,  3) (-1,  1),
    Point ( 0,  5) ( 0, -1),
    Point (-2,  2) ( 2,  0),
    Point ( 5, -2) ( 1,  2),
    Point ( 1,  4) ( 2,  1),
    Point (-2,  7) ( 2, -2),
    Point ( 3,  6) (-1, -1),
    Point ( 5,  0) ( 1,  0),
    Point (-6,  0) ( 2,  0),
    Point ( 5,  9) ( 1, -2),
    Point (14,  7) (-2,  0),
    Point (-3,  6) ( 2, -1)]


input :: [Point]
input = [
    Point ( 21518, -21209) (-2,  2),
    Point ( 10842,  21423) (-1, -2),
    Point ( 32189, -21209) (-3,  2),
    Point (-21158, -21218) ( 2,  2),
    Point (-31794, -53194) ( 3,  5),
    Point (-42469,  42743) ( 4, -4),
    Point (-53120, -31873) ( 5,  3),
    Point ( 32177, -42536) (-3,  4),
    Point ( 53505,  32084) (-5, -3),
    Point (-53091,  10764) ( 5, -1),
    Point (-53141, -21211) ( 5,  2),
    Point (-42429,  10764) ( 4, -1),
    Point (-10492, -31873) ( 1,  3),
    Point ( 42819, -10550) (-4,  1),
    Point (-53096, -10551) ( 5,  1),
    Point (-10492, -42536) ( 1,  4),
    Point (-10508,  42739) ( 1, -4),
    Point (-10471, -42533) ( 1,  4),
    Point ( 53446,  32086) (-5, -3),
    Point ( 10820, -53195) (-1,  5),
    Point ( 10858,  21423) (-1, -2),
    Point (-31810, -10553) ( 3,  1),
    Point (-10492,  32077) ( 1, -3),
    Point (-31799, -42527) ( 3,  4),
    Point (-10484,  42738) ( 1, -4),
    Point ( 10862,  10767) (-1, -1),
    Point (-10503,  10759) ( 1, -1),
    Point (-21130,  32081) ( 2, -3),
    Point (-42477, -31877) ( 4,  3),
    Point (-10452, -42530) ( 1,  4),
    Point (-10452,  53399) ( 1, -5),
    Point (-31826,  42744) ( 3, -4),
    Point (-42444, -10556) ( 4,  1),
    Point ( 42824, -31873) (-4,  3),
    Point (-10492,  32082) ( 1, -3),
    Point (-53092,  10759) ( 5, -1),
    Point ( 21522,  42745) (-2, -4),
    Point ( 53474, -42532) (-5,  4),
    Point (-31822,  42737) ( 3, -4),
    Point ( 32131, -42529) (-3,  4),
    Point (-10476, -53193) ( 1,  5),
    Point ( 21473, -10551) (-2,  1),
    Point (-10497,  21422) ( 1, -2),
    Point ( 21469,  42741) (-2, -4),
    Point ( 21470,  10763) (-2, -1),
    Point (-31807,  32086) ( 3, -3),
    Point ( 32144,  32078) (-3, -3),
    Point (-53120, -42531) ( 5,  4),
    Point (-31773, -31871) ( 3,  3),
    Point (-31800, -21218) ( 3,  2),
    Point ( 53478, -21213) (-5,  2),
    Point ( 21498,  21424) (-2, -2),
    Point ( 42798,  32081) (-4, -3),
    Point ( 42790,  10761) (-4, -1),
    Point (-31778, -10558) ( 3,  1),
    Point (-21106,  21418) ( 2, -2),
    Point (-31785,  53401) ( 3, -5),
    Point ( 53497, -42536) (-5,  4),
    Point ( 53491, -42535) (-5,  4),
    Point (-21115, -21218) ( 2,  2),
    Point ( 53478,  53401) (-5, -5),
    Point ( 10835,  32077) (-1, -3),
    Point (-10500,  21421) ( 1, -2),
    Point (-53142, -21212) ( 5,  2),
    Point (-10490, -31868) ( 1,  3),
    Point ( 53506,  21419) (-5, -2),
    Point ( 21517,  10764) (-2, -1),
    Point ( 32136,  32082) (-3, -3),
    Point (-42485, -42536) ( 4,  4),
    Point ( 10821, -53195) (-1,  5),
    Point (-42445, -10551) ( 4,  1),
    Point (-10473, -21214) ( 1,  2),
    Point ( 42806, -53186) (-4,  5),
    Point ( 42791,  10767) (-4, -1),
    Point (-53144,  32083) ( 5, -3),
    Point (-53096,  53402) ( 5, -5),
    Point (-21119,  42738) ( 2, -4),
    Point ( 42824,  53404) (-4, -5),
    Point ( 21495,  32081) (-2, -3),
    Point (-31782,  21421) ( 3, -2),
    Point (-21157, -31873) ( 2,  3),
    Point ( 53478,  32083) (-5, -3),
    Point ( 53470,  53398) (-5, -5),
    Point ( 42797,  21422) (-4, -2),
    Point (-21143, -31877) ( 2,  3),
    Point ( 32144, -21216) (-3,  2),
    Point (-10482,  32086) ( 1, -3),
    Point ( 42831, -21216) (-4,  2),
    Point (-42448,  53397) ( 4, -5),
    Point (-21135, -21213) ( 2,  2),
    Point ( 42806,  32086) (-4, -3),
    Point ( 42805,  53404) (-4, -5),
    Point (-53096,  32080) ( 5, -3),
    Point ( 32155,  32077) (-3, -3),
    Point ( 53463, -10550) (-5,  1),
    Point ( 53475,  42737) (-5, -4),
    Point ( 21470, -31873) (-2,  3),
    Point (-10452, -10550) ( 1,  1),
    Point ( 10850,  21427) (-1, -2),
    Point ( 10854,  53398) (-1, -5),
    Point (-10500,  53395) ( 1, -5),
    Point ( 21497,  10759) (-2, -1),
    Point ( 32155, -42527) (-3,  4),
    Point ( 53505, -42534) (-5,  4),
    Point (-21142,  53404) ( 2, -5),
    Point ( 32176, -53193) (-3,  5),
    Point (-42437, -53193) ( 4,  5),
    Point (-21163,  53396) ( 2, -5),
    Point ( 32128, -42531) (-3,  4),
    Point ( 42798, -53195) (-4,  5),
    Point ( 42829,  32081) (-4, -3),
    Point ( 21510, -53192) (-2,  5),
    Point (-53108, -21214) ( 5,  2),
    Point (-21124,  53399) ( 2, -5),
    Point (-21108, -31875) ( 2,  3),
    Point (-31794,  10768) ( 3, -1),
    Point ( 10842,  21426) (-1, -2),
    Point (-53142,  21424) ( 5, -2),
    Point (-42476,  53399) ( 4, -5),
    Point ( 42816, -10554) (-4,  1),
    Point ( 32179,  42741) (-3, -4),
    Point ( 32171,  32081) (-3, -3),
    Point (-10503,  32077) ( 1, -3),
    Point ( 42813,  42745) (-4, -4),
    Point ( 21485,  42736) (-2, -4),
    Point (-42444,  10762) ( 4, -1),
    Point (-31789, -53187) ( 3,  5),
    Point (-10480,  32077) ( 1, -3),
    Point ( 32128, -10556) (-3,  1),
    Point ( 10858, -31869) (-1,  3),
    Point (-53144, -42532) ( 5,  4),
    Point (-42461, -21215) ( 4,  2),
    Point (-42457, -10550) ( 4,  1),
    Point ( 32186,  32083) (-3, -3),
    Point (-42440,  53403) ( 4, -5),
    Point (-21130,  21419) ( 2, -2),
    Point ( 42819, -21216) (-4,  2),
    Point (-31810, -31874) ( 3,  3),
    Point (-53120,  32080) ( 5, -3),
    Point ( 32178, -42536) (-3,  4),
    Point ( 21509, -31876) (-2,  3),
    Point (-42436, -31868) ( 4,  3),
    Point (-42432, -21211) ( 4,  2),
    Point (-21155,  21422) ( 2, -2),
    Point (-53111, -42532) ( 5,  4),
    Point ( 42819, -53188) (-4,  5),
    Point (-21147, -21209) ( 2,  2),
    Point ( 42840,  42741) (-4, -4),
    Point (-31782,  32079) ( 3, -3),
    Point (-42448, -31871) ( 4,  3),
    Point ( 21501,  53398) (-2, -5),
    Point (-31797,  21426) ( 3, -2),
    Point (-42448,  53402) ( 4, -5),
    Point (-53110, -21214) ( 5,  2),
    Point ( 32179,  32086) (-3, -3),
    Point (-21138,  32080) ( 2, -3),
    Point ( 10871,  32086) (-1, -3),
    Point ( 53470,  32082) (-5, -3),
    Point (-53141, -53193) ( 5,  5),
    Point ( 53462, -31868) (-5,  3),
    Point (-21116, -42531) ( 2,  4),
    Point (-31782, -21212) ( 3,  2),
    Point (-42477, -31874) ( 4,  3),
    Point ( 42836, -21218) (-4,  2),
    Point (-31826, -53193) ( 3,  5),
    Point ( 10818,  42740) (-1, -4),
    Point ( 53503,  10763) (-5, -1),
    Point (-21109, -21215) ( 2,  2),
    Point (-21143,  53403) ( 2, -5),
    Point (-53120,  32078) ( 5, -3),
    Point (-31794, -10550) ( 3,  1),
    Point (-42473,  42740) ( 4, -4),
    Point (-31782,  53398) ( 3, -5),
    Point (-53120, -21218) ( 5,  2),
    Point (-53119,  53399) ( 5, -5),
    Point (-10500,  53404) ( 1, -5),
    Point ( 53503,  53399) (-5, -5),
    Point ( 21498,  53396) (-2, -5),
    Point (-31766,  42737) ( 3, -4),
    Point ( 32138, -42532) (-3,  4),
    Point ( 32133, -10559) (-3,  1),
    Point ( 53505,  42738) (-5, -4),
    Point ( 21518,  21418) (-2, -2),
    Point (-53144,  53399) ( 5, -5),
    Point (-31810, -42535) ( 3,  4),
    Point (-53099, -53187) ( 5,  5),
    Point (-31805, -42527) ( 3,  4),
    Point ( 21478,  42740) (-2, -4),
    Point ( 53506,  53403) (-5, -5),
    Point (-21165, -42533) ( 2,  4),
    Point ( 10850, -21217) (-1,  2),
    Point (-21143, -42528) ( 2,  4),
    Point (-31769,  42740) ( 3, -4),
    Point (-53088, -21211) ( 5,  2),
    Point (-53119, -31868) ( 5,  3),
    Point (-42448,  32084) ( 4, -3),
    Point (-53096, -31872) ( 5,  3),
    Point (-53091, -53186) ( 5,  5),
    Point ( 42787,  10760) (-4, -1),
    Point ( 10861,  21427) (-1, -2),
    Point ( 53446,  53402) (-5, -5),
    Point ( 42832,  10759) (-4, -1),
    Point ( 21485, -42535) (-2,  4),
    Point (-10482, -42532) ( 1,  4),
    Point ( 53483, -53194) (-5,  5),
    Point ( 10835, -31868) (-1,  3),
    Point ( 53507, -10550) (-5,  1),
    Point ( 42815,  32077) (-4, -3),
    Point ( 10847, -42533) (-1,  4),
    Point (-42448,  32084) ( 4, -3),
    Point (-21117,  32086) ( 2, -3),
    Point (-21167,  42742) ( 2, -4),
    Point (-31826,  21427) ( 3, -2),
    Point ( 32181,  42745) (-3, -4),
    Point ( 32136,  10766) (-3, -1),
    Point (-21138,  32085) ( 2, -3),
    Point ( 21501, -21218) (-2,  2),
    Point ( 21510, -10557) (-2,  1),
    Point (-31777,  21427) ( 3, -2),
    Point (-53084,  21426) ( 5, -2),
    Point (-31810,  21426) ( 3, -2),
    Point (-31797, -10556) ( 3,  1),
    Point (-31794,  42744) ( 3, -4),
    Point (-31773, -42529) ( 3,  4),
    Point ( 10818,  32084) (-1, -3),
    Point (-21149, -21209) ( 2,  2),
    Point ( 10855,  32078) (-1, -3),
    Point ( 32186, -21215) (-3,  2),
    Point ( 53454,  42743) (-5, -4),
    Point ( 21530,  10759) (-2, -1),
    Point ( 42795,  53404) (-4, -5),
    Point ( 21522,  32078) (-2, -3),
    Point (-21111, -42535) ( 2,  4),
    Point (-31826,  32079) ( 3, -3),
    Point (-21133,  10763) ( 2, -1),
    Point (-10499,  32077) ( 1, -3),
    Point ( 32170, -21213) (-3,  2),
    Point (-53096,  10765) ( 5, -1),
    Point ( 42839, -10554) (-4,  1),
    Point (-53101,  53400) ( 5, -5),
    Point (-31782, -21212) ( 3,  2),
    Point (-10508, -10556) ( 1,  1),
    Point (-53142, -31874) ( 5,  3),
    Point ( 53479,  53399) (-5, -5),
    Point (-31818, -31871) ( 3,  3),
    Point (-31789, -21217) ( 3,  2),
    Point ( 32186,  10765) (-3, -1),
    Point (-42427, -21215) ( 4,  2),
    Point (-21157,  10759) ( 2, -1),
    Point (-10448,  42737) ( 1, -4),
    Point ( 21493, -10550) (-2,  1),
    Point ( 42819, -10557) (-4,  1),
    Point ( 42830,  10764) (-4, -1),
    Point (-31794, -21211) ( 3,  2),
    Point (-21139, -10555) ( 2,  1),
    Point (-42441,  10766) ( 4, -1),
    Point ( 32176,  42740) (-3, -4),
    Point ( 32136,  53401) (-3, -5),
    Point (-10484,  42737) ( 1, -4),
    Point (-21138,  32079) ( 2, -3),
    Point (-10508,  10759) ( 1, -1),
    Point ( 21522,  53396) (-2, -5),
    Point (-31818,  21419) ( 3, -2),
    Point ( 10867, -21213) (-1,  2),
    Point ( 21469,  21424) (-2, -2),
    Point ( 32139, -53191) (-3,  5),
    Point ( 42819, -21214) (-4,  2),
    Point ( 42803, -42534) (-4,  4),
    Point ( 10847,  53398) (-1, -5),
    Point (-42437, -42528) ( 4,  4),
    Point ( 21521, -42531) (-2,  4),
    Point ( 42835,  32080) (-4, -3),
    Point (-10503,  21427) ( 1, -2),
    Point (-21167, -42527) ( 2,  4),
    Point ( 21521,  32077) (-2, -3),
    Point (-10484, -10553) ( 1,  1),
    Point (-10452, -42529) ( 1,  4),
    Point ( 10847, -21213) (-1,  2),
    Point ( 53470,  21419) (-5, -2),
    Point ( 10862, -10551) (-1,  1),
    Point (-42469, -31868) ( 4,  3),
    Point ( 10838, -10550) (-1,  1),
    Point ( 32152, -42527) (-3,  4),
    Point ( 32176,  53401) (-3, -5),
    Point (-31825, -10554) ( 3,  1),
    Point (-21167, -42528) ( 2,  4),
    Point (-21115, -31872) ( 2,  3),
    Point ( 10866, -21217) (-1,  2),
    Point ( 21506, -10555) (-2,  1),
    Point (-21143,  53399) ( 2, -5),
    Point (-10471, -31877) ( 1,  3),
    Point (-10508,  32085) ( 1, -3),
    Point ( 10866, -10550) (-1,  1),
    Point (-53088, -31877) ( 5,  3),
    Point (-10495,  10759) ( 1, -1),
    Point ( 10847,  21418) (-1, -2),
    Point (-10484,  42743) ( 1, -4),
    Point ( 10818,  10764) (-1, -1),
    Point ( 32155,  42740) (-3, -4),
    Point ( 42787,  21423) (-4, -2),
    Point ( 53490,  21425) (-5, -2),
    Point (-42448,  10764) ( 4, -1),
    Point (-21119,  53396) ( 2, -5),
    Point ( 21528, -21211) (-2,  2),
    Point ( 53502,  10762) (-5, -1),
    Point ( 42820, -31873) (-4,  3),
    Point ( 21481,  53395) (-2, -5),
    Point (-10468, -42527) ( 1,  4),
    Point ( 32130, -21212) (-3,  2),
    Point ( 32181, -10554) (-3,  1),
    Point ( 32184,  53395) (-3, -5),
    Point ( 10851, -53188) (-1,  5),
    Point ( 21477,  42736) (-2, -4),
    Point (-21159,  21426) ( 2, -2),
    Point (-31773,  10767) ( 3, -1),
    Point ( 42827,  32077) (-4, -3),
    Point (-31818, -21210) ( 3,  2),
    Point ( 53458, -21218) (-5,  2),
    Point (-10452,  42740) ( 1, -4),
    Point (-53142, -21215) ( 5,  2),
    Point ( 10826,  53398) (-1, -5),
    Point (-21166,  42741) ( 2, -4),
    Point (-42469, -53187) ( 4,  5),
    Point ( 32173,  10768) (-3, -1),
    Point ( 53497,  10759) (-5, -1),
    Point ( 53449, -10557) (-5,  1),
    Point ( 32170, -53190) (-3,  5),
    Point ( 42843, -53187) (-4,  5),
    Point ( 10852,  32081) (-1, -3),
    Point (-10499,  53395) ( 1, -5),
    Point ( 21496, -53195) (-2,  5),
    Point ( 32157,  32084) (-3, -3),
    Point ( 42831,  10765) (-4, -1),
    Point ( 21498, -10556) (-2,  1),
    Point ( 42843, -53193) (-4,  5),
    Point ( 42811, -53189) (-4,  5),
    Point (-42477,  53397) ( 4, -5)]
