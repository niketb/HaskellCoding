Below we have some mathematical binary arguments that you may recognize from homework 2.

> data Binop =
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   deriving (Show)

applyOp performs these operations, but unlike in the homework,
you now must consider errors (represented by 'Nothing').

> applyOp :: Binop -> Maybe Int -> Maybe Int -> Maybe Int

Plus is done for you.  Notice how code must check for 'Nothing'
for each operand.

> applyOp Plus mi mj =
>   case mi of
>     Nothing -> Nothing               
>     Just i ->
>       case mj of
>         Nothing -> Nothing
>         Just j -> Just $ i + j

Minus is also done for you.  This case **does** use monads,
but without the do syntax.

> applyOp Minus mi mj =
>   mi >>= (\i -> mj >>= (\j -> Just $ i - j))

What's happening here? The bind operator >>= is unboxing mi from it's data structure (e.g., maybe) and binding it to i in the anonymous function that follows. This i now acts as the argument for the anonymous function mj... In this anon func, the mj is again unbound and passed to the second anon func that is nested within the first one. Within this second anon func, both i and and j are not accessible. They are added and then boxed in Just. Notice how, given the intelligence of the bind operator, that big plus program is reduced to one line.

Implement Times and Divide.  Try the Times case without using bind (>>=).

> applyOp Times mi mj = 
>     case mi of
>     Nothing -> Nothing
>     Just i -> 
>       case mj of 
>           Nothing -> Nothing
>           Just j -> Just $ i * j  

For the Divide case, use bind (>>=) as we did for Minus.
On an attempt to divide by 0, return Nothing as the answer.

> applyOp Divide mi mj =
>     mi >>= (\i -> 
>     mj >>= (\j -> 
>     if j > 0  
>         then Just $ i `div` j
>         else fail "One does not simply divide by 0"))

fail is like an intelligent version of error, which gives you an answer appropriate to the data structure you're using (e.g., Nothing, for a Maybe data structure) 

The following test cases will help you verify your changes.

> testapp1 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (Just 2)
> testapp2 = applyOp Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (applyOp Plus (Just 3) (Just (-3)))


Now implement applyOp', which implements all methods using the do syntax.
The Plus case is done for you once again.  Be sure to check for zero with Divide.

> applyOp' :: Binop -> Maybe Int -> Maybe Int -> Maybe Int
> applyOp' Plus mi mj = do
>   i <- mi
>   j <- mj
>   return $ i + j
> applyOp' Minus mi mj = do
>     i <- mi
>     j <- mj
>     return $ i - j
> applyOp' Times mi mj = do
>     i <- mi
>     j <- mj
>     return $ i * j
> applyOp' Divide mi mj = do
>     i <- mi
>     j <- mj
>     if j > 0
>         then return $ i `div` j
>         else fail "One does not simply divide by 0"

Here, <- is unboxing, and return is boxing. Otherwise it works like bind. The do helps you execute multiple sequential statements (check this).

You have to define a monad to work with a data structure. It's already defined for you for the maybe and either data structures. But for say, a tree structure, you'll have to define it. You'll also have to define how fail works for it.

More test cases:

> testapp1' = applyOp' Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (Just 2)
> testapp2' = applyOp' Minus (applyOp Times (Just 3) (Just 4)) $ applyOp Divide (Just 8) (applyOp Plus (Just 3) (Just (-3)))


Finally, note the following function for incrementing and decrementing ints.

> mincr :: Int -> Maybe Int
> mincr i = Just $ i + 1

> mdecr :: Int -> Maybe Int
> mdecr i = Just $ i - 1

Experiment with these functions and the >>= syntax.
Here is one example:

> testIncDec = Just 7 >>= mincr >>= mincr >>= mincr >>= mdecr
> testIncDec1 = Just 9 >>= mdecr >>= mdecr >>= mdecr >>=  mdecr
> testIncDec2 = do
>      i <- Just 9
>      j <- mdecr i
>      k <- mdecr j
>      l <- mdecr k
>      m <- mdecr l
>      return m

Does bind seem more natural in this case than using do?  Why or why not?
Yes, bind seems more natural in this case, because here, you just want to carry out a sequence of steps without saving intermediate results. Bind allows you to do that. It's not something you miss in non-functional languages, because there you can update the same variable over and over (mutable references). That's not the case with Haskell. 