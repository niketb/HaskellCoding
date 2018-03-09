This File is _literate Haskell_.
That means that (in some sense) code and comments are reversed.
By default, everything that I type is actually a comment.

To write code, I preface it with a 'greater than' symbol.
Here we define the expressions in our language:

> data Exp = ETrue
>          | EFalse
>          | Eif Exp Exp Exp
>		   | EInt Integer
>		   | EPred Exp
>		   | ESucc Exp	
>   deriving Show

When an expression is evaluated, it returns a value.

> data Val = VTrue
>          | VFalse
>          | VInt Integer
>		   | VPred Val
>		   | VSucc Val 
>   deriving Show

The evaluate function takes an expression and returns a value
The VTrue case has been done for you.
You must complete the other cases.

> evaluate :: Exp -> Val
> evaluate ETrue = VTrue
> evaluate EFalse = VFalse
> evaluate (Eif cond expTrue expFalse)
>	| result `vEquals` VTrue = evaluate expTrue
>	| result `vEquals` VFalse = evaluate expFalse
>	| otherwise = error "Invalid operation: a number cannot be used as a condition"
>	where result = evaluate cond
> evaluate (EInt n) = VInt ((read (show n) :: Integer)) 
> evaluate (EPred exp) = vPred (evaluate exp) 
> evaluate (ESucc exp) = vSucc (evaluate exp)

> vEquals :: Val -> Val -> Bool
> vEquals VTrue VTrue = True
> vEquals VFalse VFalse = True
> vEquals VFalse VTrue = False

> vPred :: Val -> Val
> vPred VTrue = error "Invalid operation: VTrue has no predecessor"
> vPred VFalse = error "Invalid operation: VFalse has no predecessor"
> vPred	(VInt v) = VInt ((read (show v) :: Integer) - 1)

> vSucc :: Val -> Val
> vSucc VTrue = error "Invalid operation: VTrue has no successor"
> vSucc VFalse = error "Invalid operation: VFalse has no successor"
> vSucc	(VInt v) = VInt ((read (show v) :: Integer) + 1)

And here we have a couple of programs to test.
prog1 should evaluate to VTrue and prog2 should evaluate to VFalse

> prog1 = Eif ETrue ETrue EFalse
> prog2 = Eif (Eif ETrue EFalse ETrue) ETrue (Eif ETrue EFalse ETrue)
> prog3 = Eif ETrue (EInt 5) (EInt 8)
> prog4 = Eif ETrue (ESucc (EInt 5)) (EInt 8)
> prog5 = Eif EFalse (EInt 5) (EPred (EInt 8))
> prog6 = Eif (Eif ETrue EFalse ETrue) (EInt 5) (EInt 8)
> prog7 = EPred (Eif ETrue (EInt 10) (EInt 1))
> prog8 = EPred (EPred (EInt 12)) 
> prog9 = ESucc (EPred (EInt (-11)))
> prog10 = ESucc (Eif ETrue EFalse ETrue)

The following lines evaluate the test expressions and display the results.
Note the type of main.  'IO ()' indicates that the function performs IO and returns nothing.
The word 'do' begins a block of code, were you can effectively do sequential statements.
(This is a crude generalization, but we'll talk more about what is going on in this function
when we deal with the great and terrible subject of _monads_.)

> main :: IO ()
> main = do
>   putStrLn $ "Evaluating '" ++ (show prog1) ++ "' results in " ++ (show $ evaluate prog1)
>   putStrLn $ "Evaluating '" ++ (show prog2) ++ "' results in " ++ (show $ evaluate prog2)
>   putStrLn $ "Evaluating '" ++ (show prog3) ++ "' results in " ++ (show $ evaluate prog3)
>   putStrLn $ "Evaluating '" ++ (show prog4) ++ "' results in " ++ (show $ evaluate prog4)
>   putStrLn $ "Evaluating '" ++ (show prog5) ++ "' results in " ++ (show $ evaluate prog5)
>   putStrLn $ "Evaluating '" ++ (show prog6) ++ "' results in " ++ (show $ evaluate prog6)
>   putStrLn $ "Evaluating '" ++ (show prog7) ++ "' results in " ++ (show $ evaluate prog7)
>   putStrLn $ "Evaluating '" ++ (show prog8) ++ "' results in " ++ (show $ evaluate prog8)
>   putStrLn $ "Evaluating '" ++ (show prog9) ++ "' results in " ++ (show $ evaluate prog9)
>   putStrLn $ "Evaluating '" ++ (show prog10) ++ "' results in " ++ (show $ evaluate prog10)


Once you have the evaluate function working
you add in support the expressions 'succ e', 'pred e', and 'zero'.
With this change, it is possible for evaluate to get 'stuck',
e.g. pred true.
For a first pass, simply use the error function in these cases.