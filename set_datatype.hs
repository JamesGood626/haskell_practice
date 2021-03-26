data Set a = Leaf | Branch (Set a) a (Set a)
  deriving (Eq, Ord)
  
empty :: Set a
empty = Leaf

{-
Ord typeclass looks something along the lines of:

class Ord a where
  compare :: a -> a -> Ordering
  
-}

{-
instance Ord (Tree a) where
  compare (Branch Leaf x Leaf) (Branch Leaf y Leaf) = GT
-}
-- if y < x then LT else GT

{-
  An error I ran into... 
  And really want compare to look like this:
  compare (Set (Branch Leaf x Leaf)) = ...
  compare (Set (Branch (Branch) x Leaf)) = ...
  ... etc, need a recursion scheme to visit All
  nodes of the tree.

  main.hs:13:10: error:
    * Illegal instance declaration for `Ord (Set (Tree a))'
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    * In the instance declaration for `Ord (Set (Tree a))'
   |
13 | instance Ord (Set (Tree a)) where
   |
-}

-- recursion scheme from Algs in Haskell:
-- flatten t = flatcat t []
-- flatcat Null xs = xs
-- flatcat (Node l x r) xs = flatcat l (x : flatcat r xs)

instance Show a => Show (Set a) where
  show Leaf = show "Leaf"
  show (Branch Leaf x Leaf) = show x
  show (Branch lb x rb) = "(" ++ show lb ++ "-" ++ show x ++ "-" ++ show rb ++ ")"

singleton :: a -> Set a
singleton x = Branch Leaf x Leaf

{-
  Example Usage:

    insert 100 empty
    => 100
      insert 200 $ insert 100 empty
    => "Leaf"-100-200
      insert 300 $ insert 200 $ insert 100 empty
    => "Leaf"-100-"Leaf"-200-300
    insert 60 $ insert 40 $ insert 50 $ insert 300 $ insert 200 $ insert 100 empty
    => ((40-50-60)-100-("Leaf"-200-300))
-}
insert :: Ord a => a -> Set a -> Set a
insert n Leaf = Branch Leaf n Leaf
insert n (Branch lb x rb) =
  case n < x of
    True -> (Branch (insert n lb) x rb)
    False -> (Branch lb x (insert n rb))

{-
  Example Usage:
  
  member (insert 60 $ insert 40 $ insert 50 $ insert 300 $ insert 200 $ insert 100 empty) 100
  => True
  member (insert 60 $ insert 40 $ insert 50 $ insert 300 $ insert 200 $ insert 100 empty) 700
  => False
-}
member :: Ord a => Set a -> a -> Bool
member Leaf n = False
member (Branch lb x rb) n =
  if x == n then
    True
  else
    case n < x of
      True -> member lb n
      False -> member rb n

{-
elements (Branch lb x rb) = (elements lb):[x, (elements rb)]
Got an error with the above:
main.hs:97:44: error:
    * Occurs check: cannot construct the infinite type: a ~ [a]
    * In the expression: x
      In the second argument of `(:)', namely `[x, (elements rb)]'
      In the expression: (elements lb) : [x, (elements rb)]
    * Relevant bindings include
        rb :: Set a (bound at main.hs:97:23)
        x :: a (bound at main.hs:97:21)
        lb :: Set a (bound at main.hs:97:18)
        elements :: Set a -> [a] (bound at main.hs:96:1)
   |
97 | elements (Branch lb x rb) = (elements lb):[x, (elements rb)]

Example Usage:
s = insert 60 $ insert 40 $ insert 50 $ insert 300 $ insert 200 $ insert 100 empty
s
=> ((40-50-60)-100-("Leaf"-200-300))
elements s
=> [40,50,60,100,200,300]
-}
elements :: Set a -> [a]
elements Leaf = []
elements (Branch lb x rb) = (elements lb) ++ [x] ++ (elements rb)



-- Not quite sure what the merge behavior should be like.
-- Use the elements function and merge the two lists of the Sets
-- followed by sorting them from greatest to least and then insert
-- all of them into a Set?
-- 1. call elements on one of the sets
-- 2. reduce over the lists returned by elements, calling insert into the other set
--    for each list item.
{-
  Implemented it on the first try, Let's Go!!!!!
  Exmaple Usage:

  s = insert 60 $ insert 40 $ insert 50 $ insert 300 $ insert 200 $ insert 100 empty
  s
  => ((40-50-60)-100-("Leaf"-200-300))
  s2 = insert 5000 $ insert 4000 $ insert 2000 empty
  s2
  => ("Leaf"-2000-("Leaf"-4000-5000))
  merge s s2
  => ((((((40-50-"Leaf")-60-"Leaf")-100-"Leaf")-200-"Leaf")-300-"Leaf")-2000-("Leaf"-4000-5000))
-}
merge :: Ord a => Set a -> Set a -> Set a
merge x y = foldr insert y xs
  where xs = elements x

-- Was curious if this was possible... It's Not
-- combine :: Integer -> (Bool | Integer)
-- combine x = if x === 0 then True else 100

main = pure ()

-- For later reference:
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Foldable.html
-- data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
-- instance Foldable Tree where
--   foldr f z Empty = z
--   foldr f z (Leaf x) = f x z
--   foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
