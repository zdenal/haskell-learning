module NTreeZipper where
  import  ListZipper (Zipper(Zip), cursor, right, left, replace)

  {-
    Node has Value and children (zipper for inspecting them)
  -}
  data ZNode a = Node a (Zipper (ZNode a)) deriving (Show)

  {-
    Tree is composed from list of zippers (parent levels) and current level (zipper)
  -}
  data NTree a = Tree [Zipper (ZNode a)] (Zipper (ZNode a)) deriving (Show)

  nodeValue :: ZNode a -> a
  nodeValue (Node a _) = a

  currentNode :: NTree a -> ZNode a
  currentNode (Tree _ zipper) = cursor zipper

  createNode :: a -> ZNode a
  createNode a = Node a (Zip [] [])

  replaceValue :: ZNode a -> a -> ZNode a
  replaceValue (Node _ children) a = Node a children

  value :: NTree a -> a
  value = nodeValue . currentNode

  right :: NTree a -> NTree a
  right (Tree parents zipper) = Tree parents $ ListZipper.right zipper

  left :: NTree a -> NTree a
  left (Tree parents zipper) = Tree parents $ ListZipper.left zipper

  replace :: NTree a -> a -> NTree a
  replace tree@(Tree parents zipper) a =
    let node = currentNode tree
    in Tree parents $ ListZipper.replace zipper $ replaceValue node a

  {-
    Insert new node to current position
  -}
  insert :: a -> NTree a -> NTree a
  insert a (Tree parents (Zip ls rs)) = Tree parents (Zip ls ((createNode a):rs))

  {-
    Deletes node at current cursor/position with children.
    The next one on the right becomes the current position.
  -}
  delete :: NTree a -> NTree a
  delete (Tree parents (Zip ls (x:rs))) = Tree parents (Zip ls rs)

  children :: NTree a -> NTree a
  children (Tree parents (Zip ls ((Node a children):rs))) =
    {-
      Create parent level from current w/o children (zipper) as they are now current level
    -}
    let parentLevel = Zip ls ((createNode a):rs)
    in Tree (parentLevel:parents) children

  parent :: NTree a -> NTree a
  parent (Tree ((Zip ls ((Node a _):rs)):parents) zipper) =
    {-
      Put back to prev parent level current level (zipper)
    -}
    let parentLevel = Zip ls ((Node a zipper):rs)
    in Tree parents parentLevel
