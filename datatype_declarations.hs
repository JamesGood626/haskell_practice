data Shape = Circle Int | Square Int Int
{-
  > :t Circle
  Circle :: Int -> Shape
  > :t Square
  Square :: Int -> Int -> Shape
  > :t Square 1
  Square 1 :: Int -> Shape
  > :t Square 1 2
  Square 1 2 :: Shape
  > :t Circle 2
  Circle 2 :: Shape

  Other data types:
  > :t []
  [] :: [a]
  > :t (:)
  (:) :: a -> [a] -> [a]
-}

{-
  Able to use the Maybe type constructor in a function's
  type signature, however, using the type constructor to
  pattern match within the function head is a no go.
-}
data Stack x xs = Empty | OnTop x xs
                deriving Show

instance (Show x xs) => Show (Stack x xs) where
  show Empty = "empty"
  show (OnTop x xs) = "OnTop: " ++ x ++ "Rest: " ++ show xs

{-
  The above Stack and instance (Show x xs) declaration
  was a show stopper

  The error:
  <interactive>:4:11: error:
    * Expected kind `* -> Constraint',
        but `Show x' has kind `Constraint'
    * In the instance declaration for `Show (Stack x xs)'
Prelude>   show Empty = "empty"
Prelude>   show (OnTop x xs) = "OnTop: " ++ x ++ "Rest: " ++ show xs

<interactive>:6:3: error:
    * Occurs check: cannot construct the infinite type:
        t ~ Stack [Char] t
      Expected type: t -> [Char]
        Actual type: Stack [Char] t -> [Char]
    * Relevant bindings include
        show :: t -> [Char] (bound at <interactive>:6:3)
-}

isEmpty :: Stack a -> Bool
isEmpty Empty = True

pop :: Stack a -> a
pop (OnTop a) = a

push :: x -> [x] -> [x]
push x [] = x:[]
push x xs = x:xs

peek :: [x] -> Maybe x
peek [] = Nothing
peek (x:xs) = Just x

-- ALAS AFTER TRIAL AND ERROR!! --
data Shape = Circle Int | Square Int Int
{-
  > :t Circle
  Circle :: Int -> Shape
  > :t Square
  Square :: Int -> Int -> Shape
  > :t Square 1
  Square 1 :: Int -> Shape
  > :t Square 1 2
  Square 1 2 :: Shape
  > :t Circle 2
  Circle 2 :: Shape

  Other data types:
  > :t []
  [] :: [a]
  > :t (:)
  (:) :: a -> [a] -> [a]
-}

{-
  Able to use the Maybe type constructor in a function's
  type signature, however, using the type constructor to
  pattern match within the function head is a no go.
-}

{-
  Example Usage:

  OnTop 100 $ OnTop 10 $ Empty
  => OnTop 100 (OnTop 10 Empty)

  data Stack x xs = Empty | OnTop x xs
                deriving Show
-}

{-
Why this no work, but deriving Show alone does?
instance Show (Stack x xs) where
  show Empty = "empty"
  show (OnTop x xs) = "OnTop: " ++ x ++ "Rest: " ++ show xs

error says somethign about [Char] can't ++ with x rigid data type.
x is a generic so the ++ can't work on it because it's not guaranteed to be
a String, so is it necessary to do use a function to convert the datatype to
a String if it's possible, or fail/return Maybe otherwise?
-}

{-
  Example Usage:
    isEmpty $ OnTop 100 $ OnTop 10 $ Empty
    => False
    isEmpty $ Empty
    => True

isEmpty :: Stack x xs -> Bool
isEmpty Empty = True
isEmpty (OnTop _ _) = False
-}


{-
pop :: Stack x xs -> Maybe (x, Stack x xs)
pop Empty = Nothing
pop (OnTop x xs) = Just (x, xs)
-}

{-
pop :: Stack x xs -> Maybe (x, Stack x xs)
pop Empty = Nothing
pop (OnTop x xs) = Just (x, xs)

This function definition seems natural... as xs I've coded Stack x xs with the
intention that xs will only every be an OnTop x xs or an Empty, but this causes
the error below...

  main.hs:61:20: error:
    * Occurs check: cannot construct the infinite type: xs ~ Stack x xs
      Expected type: Maybe (x, Stack x xs)
        Actual type: Maybe (x, xs)
    * In the expression: Just (x, xs)
      In an equation for `pop': pop (OnTop x xs) = Just (x, xs)
    * Relevant bindings include
        xs :: xs (bound at main.hs:61:14)
        x :: x (bound at main.hs:61:12)
        pop :: Stack x xs -> Maybe (x, Stack x xs) (bound at main.hs:60:1)
   |
61 | pop (OnTop x xs) = Just (x, xs)
   |                    ^^^^^^^^^^^^
-}

{-
push :: x -> Stack x xs -> Stack x xs
push x Empty = OnTop x Empty
push x (OnTop y ys) = OnTop x $ OnTop y ys
-}

{-
peek :: Stack x xs -> Maybe x
peek Empty = Nothing
peek (OnTop x xs) = Just x
-}

{-
peek :: [x] -> Maybe x
peek [] = Nothing
peek (x:xs) = Just x
-}

{-
 All of the above doesn't matter... as
 the type that was asked for was
 Stack a
 NOT
 Stack x xs
-}

{-
  Example Usage:
    OnTop 100 $ OnTop 10 $ Empty
    => OnTop 100 (OnTop 10 Empty)
-}
data Stack a = Empty | OnTop a (Stack a)
            deriving Show

{-
  Example Usage:

  s = OnTop 100 $ OnTop 10 $ Empty
  isEmpty s
  => False
  isEmpty Empty
  => True
-}
isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty (OnTop _ _) = False

{-
  Example Usage:

  s = OnTop 100 $ OnTop 10 $ Empty
  s
  => OnTop 100 (OnTop 10 Empty)
  push 200 s
  => OnTop 200 (OnTop 100 (OnTop 10 Empty))
-}
push :: a -> Stack a -> Stack a
push x Empty = OnTop x Empty
push x (OnTop y ys) = OnTop x (OnTop y ys)

{-
  Example Usage
  (
    Note how returning Just (x, xs) works now
    that the data constructor for OnTop is OnTop a (Stack a)
  ):

  s = OnTop 100 $ OnTop 10 $ Empty
  pop s
  => Just (100,OnTop 10 Empty)
-}
pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (OnTop x xs) = Just (x, xs)

{-
  Example Usage:

  s = OnTop 100 $ OnTop 10 $ Empty
  peek s
  => Just 100
   s
  => OnTop 100 (OnTop 10 Empty)
-}
peek :: Stack a -> Maybe a
peek Empty = Nothing
peek (OnTop x xs) = Just x

{-
  Example Usage:

  s = OnTop 100 $ OnTop 10 $ Empty
  toList s
  => [100,10]

  This wouldn't be tail recursive though, right?
  So wouldn't it be preferable to use an accumulator?
-}
toList :: Stack a -> [a]
toList Empty = []
toList (OnTop x xs) = x:(toList xs)

{-
  Example Usage:

  xs = [100,10]
  fromList xs
  => OnTop 100 (OnTop 10 Empty)
-}
fromList :: [a] -> Stack a
fromList [] = Empty
fromList (x:xs) = OnTop x (fromList xs)

addOne :: Integer -> Integer
addOne x = x + 1

{-
  Example Usage:
  mapStack addOne s
  => OnTop 101 (OnTop 11 Empty)
-}
mapStack :: (a -> b) -> Stack a -> Stack b
mapStack f Empty = Empty
mapStack f (OnTop x xs) = OnTop (f x) (mapStack f xs)