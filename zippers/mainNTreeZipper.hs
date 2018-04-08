import NTreeZipper (
  ZNode(Node), NTree(Tree),
  currentNode, right, left,
  replace, value, delete,
  children, parent, insert)

import  ListZipper (Zipper(Zip))

main = do
  {-

  TREE

  a
  |-b
    |-d, e
  |-c
    |-f, g

  -}

  let aNode = Node 'a' (Zip [] [
                            Node 'b' (Zip [] [
                                        Node 'd' (Zip [] []),
                                        Node 'e' (Zip [] [])
                                      ]),
                            Node 'c' (Zip [] [
                                        Node 'f' (Zip [] []),
                                        Node 'g' (Zip [] [])
                                      ])
                          ])
  let tree = Tree [] (Zip [] [aNode])

  print $ currentNode $ replace tree 'g'

  {-print e => a -> b -> d -> e-}
  print $ value $ right $ children $ children tree

  {-print c-}
  print $ value $ right $ parent $ right $ children $ children tree

  {-
   - insert `i` to `b`
   - current zipper is now `>i<, d, e`
  -}
  let newTree = insert 'i' $ children $ children tree
  print $ value newTree
  let (Tree _ currentZipper) = newTree
  print currentZipper

  {-
   - delete d in newTree
   - current zipper is now `i, >e<`
  -}
  let tree2 = delete $ right newTree
  let (Tree _ currentZipper) = tree2
  print currentZipper
