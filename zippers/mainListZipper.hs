import  ListZipper (Zipper, fromList, right, left, replace, push, toList)

main = do
  let zipper = fromList [1, 2, 3]
  let a = right zipper
  let ra = replace a 10
  let b = push ra 9

  print $ toList b
