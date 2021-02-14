module Domain where

import           Text.Printf

type X = Integer
type Y = Integer
type Z = Integer

within :: Distance -> Integer -> [Integer]
within d x' = [x' - d .. x' + d]

data Position = Position
    { x :: X
    , y :: Y
    , z :: Z
    }
    deriving (Eq, Ord)

instance Show Position where
    show Position { x = x', y = y', z = z' } =
        "("
            ++ printf "%4d" x'
            ++ ", "
            ++ printf "%4d" y'
            ++ ", "
            ++ printf "%4d" z'
            ++ ")"

(+++) :: Position -> Position -> Position
u +++ v = Position (x u + x v) (y u + y v) (z u + z v)

origo :: Position
origo = Position { x = 0, y = 0, z = 0 }

type Distance = Integer

manhattan :: Position -> Position -> Distance
manhattan u v =
    let manhattan' x' y' z' = (abs x' + abs y' + abs z')
    in  manhattan' (x u - x v) (y u - y v) (z u - z v)

data Nanobot = Nanobot
    { position :: Position
    , range    :: Distance
    }
    deriving Eq

instance Show Nanobot where
    show bot = show (position bot) ++ ":" ++ printf "%3d" (range bot)

scaleDown :: Integer -> Nanobot -> Nanobot
scaleDown scale Nanobot { position = Position x' y' z', range = r } = Nanobot
    { position = Position { x = x' `div` scale
                          , y = y' `div` scale
                          , z = z' `div` scale
                          }
    , range    = r `div` scale
    }

pointsWithin :: Distance -> Position -> [Position]
pointsWithin rng pos0 =
    [ Position { x = x', y = y', z = z' }
    | x' <- within rng $ x pos0
    , y' <- within rng $ y pos0
    , z' <- within rng $ z pos0
    , manhattan Position { x = x', y = y', z = z' } pos0 < rng
    ]

data SearchCube = SearchCube
    { anchor      :: Position
    , side        :: Distance
    , botsInReach :: [Nanobot]
    }
    deriving Eq

instance Ord SearchCube where
    compare (SearchCube anchor' side' bots') (SearchCube anchor'' side'' bots'')
        | n' < n''                            = LT
        | n' == n'' && distance' > distance'' = LT
        | n' == n'' && distance' == distance'' && side' > side'' = LT
        | n' == n'' && distance' == distance'' && side' == side'' = EQ
        | otherwise                           = GT
      where
        n'         = length bots'
        distance'  = manhattan anchor' origo
        n''        = length bots''
        distance'' = manhattan anchor'' origo

instance Show SearchCube where
    show cube = printf "%3d @ %s <-> %s"
                       (side cube)
                       (show (anchor cube))
                       (show (length $ botsInReach cube))

initialCube :: [Nanobot] -> SearchCube
initialCube bots =
    let minx    = minimum $ x . position <$> bots
        maxx    = maximum $ x . position <$> bots
        miny    = minimum $ y . position <$> bots
        maxy    = maximum $ y . position <$> bots
        minz    = minimum $ z . position <$> bots
        maxz    = maximum $ z . position <$> bots
        minsize = maximum [maxx - minx, maxy - miny, maxz - minz]

        initialSize =
            let findSize :: Integer -> Integer
                findSize k =
                    if 2 ^ k > minsize then 2 ^ k else findSize (k + 1)
            in  findSize 1
    in  SearchCube
            { anchor      = Position (-initialSize `div` 2)
                                     (-initialSize `div` 2)
                                     (-initialSize `div` 2)
            , side        = initialSize
            , botsInReach = bots
            }

createCube :: [Nanobot] -> Integer -> Position -> SearchCube
createCube bots side' anchor' =
    let candidate =
            SearchCube { anchor = anchor', botsInReach = [], side = side' }
    in  candidate { botsInReach = filter (touches candidate) bots }

touches :: SearchCube -> Nanobot -> Bool
touches sq bot =
    let a          = anchor sq
        s          = side sq
        pos        = position bot

        (xlo, xhi) = (x a, x a + s - 1)
        (ylo, yhi) = (y a, y a + s - 1)
        (zlo, zhi) = (z a, z a + s - 1)


        d c lo hi | c pos < lo = lo - c pos
                  | c pos > hi = c pos - hi
                  | otherwise  = 0
        dx       = d x xlo xhi
        dy       = d y ylo yhi
        dz       = d z zlo zhi
        distance = dx + dy + dz
    in  distance <= range bot


split :: SearchCube -> [SearchCube]
split sq =
    let Position ax ay az = anchor sq
        halfSide          = side sq `div` 2
        anchors =
            [ Position ax              ay              az
            , Position (ax + halfSide) ay              az
            , Position ax              (ay + halfSide) az
            , Position (ax + halfSide) (ay + halfSide) az
            , Position ax              ay              (az + halfSide)
            , Position (ax + halfSide) ay              (az + halfSide)
            , Position ax              (ay + halfSide) (az + halfSide)
            , Position (ax + halfSide) (ay + halfSide) (az + halfSide)
            ]

        cubes = fmap (createCube (botsInReach sq) halfSide) anchors
    in  cubes
