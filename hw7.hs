--1
mapFilter :: (a -> b) -> (a -> Bool) -> [a]
mapFilter f p = map f . filter p
