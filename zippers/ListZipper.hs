module ListZipper where
  data Zipper a = Zip [a] [a] deriving (Show)

  fromList :: [a] -> Zipper a
  fromList = Zip []

  createEmpty :: Zipper a
  createEmpty = Zip [] []

  cursor :: Zipper a -> a
  cursor (Zip _ []) = error "Zipper is empty."
  cursor (Zip _ (x:_)) = x

  left :: Zipper a -> Zipper a
  left zipper@(Zip [] _) = zipper
  left (Zip (x:ls) rs) = Zip ls (x:rs)

  right :: Zipper a -> Zipper a
  right zipper@(Zip _ []) = zipper
  right (Zip ls (x:rs)) = Zip (x:ls) rs

  {-
    Question: we can do replace on empty zipper to return empty zipper w/o error raising
  -}
  replace :: Zipper a -> a -> Zipper a
  replace (Zip _ []) _ = error "Cant make replace on empty Zipper."
  replace (Zip ls (x:rs)) a = Zip ls (a:rs)

  {-
    Push item before cursor
  -}
  push :: Zipper a -> a -> Zipper a
  push (Zip ls rs) a = Zip (a:ls) rs

  {-
    remove item before cursor
  -}
  pop :: Zipper a -> Zipper a
  pop (Zip (x:ls) rs) = Zip ls rs
  pop z = z

  {-
    `++` has worse perf for very long lists
    toList :: Zipper a -> [a]
    toList (Zip ls rs) = reverse ls ++ rs
  -}

  {-
    Better perf solution for join ls & rs as `:` is better than `++`
    wo big lists. We need to flip `:` method as foldl (acc -> b -> acc) -> acc -> [b] -> acc
    and then `:` making `acc:b` where acc is list. So by flip we are correctly getting fction `b:acc`.
  -}
  toList :: Zipper a -> [a]
  toList (Zip ls rs) = foldl (flip(:)) rs ls
