module ZipList
(
    ZipList,
    current,
    fromList,
    toList,
    moveRight,
    moveLeft,
    insert,
    remove,
)
where

data ZipList a = ZipList { forward :: [a], backward :: [a] }

instance Show a => Show (ZipList a) where
  show list = show $ toList list

current :: ZipList a -> a
current (ZipList forward backward) = head forward

fromList :: [a] -> ZipList a
fromList list = ZipList list []

toList :: ZipList a -> [a]
toList (ZipList forward backward) = (reverse backward) ++ forward

moveRight :: ZipList a -> ZipList a
moveRight (ZipList (current:[]) backward)      = ZipList (reverse (current:backward)) []
moveRight (ZipList (current:forward) backward) = ZipList forward (current:backward)

moveLeft :: ZipList a -> ZipList a
moveLeft (ZipList foward [])      = let (last:reversed) = reverse foward in ZipList (last:[]) reversed
moveLeft (ZipList forward (x:backward)) = ZipList (x:forward) backward

insert :: a -> ZipList a -> ZipList a
insert value (ZipList forward backward) = ZipList (value:forward) backward

remove :: ZipList a -> ZipList a
remove (ZipList (current:[]) backward) = ZipList (reverse backward) []
remove (ZipList (current:forward) backward) = ZipList forward backward
