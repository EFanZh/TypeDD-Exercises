import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
    Push : Integer -> StackCmd () height (S height)
    Pop : StackCmd Integer (S height) height
    Top : StackCmd Integer (S height) (S height)
    GetStr : StackCmd String height height
    PutStr : String -> StackCmd () height height
    Pure : ty -> StackCmd ty height height
    (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3

runStack : (stk : Vect inHeight Integer) -> StackCmd ty inHeight outHeight -> IO (ty, Vect outHeight Integer)
runStack stk (Push val) = pure ((), val :: stk)
runStack (val :: stk) Pop = pure (val, stk)
runStack (val :: stk) Top = pure (val, val :: stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (x', newStk) <- runStack stk x
                            runStack newStk (f x')

data StackIO : Nat -> Type where
    Do : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
    (>>=) : StackCmd a height1 height2 -> (a -> Inf (StackIO height2)) -> StackIO height1
    (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run (More fuel) stk (Do c f) = do (res, newStk) <- runStack stk c
                                  run fuel newStk (f res)
run Dry stk p = pure ()

data StkInput = Number Integer | Add | Subtract | Multiply | Negate | Discard

strToInput : String -> Maybe StkInput
strToInput "" = Nothing
strToInput "add" = Just Add
strToInput "subtract" = Just Subtract
strToInput "multiply" = Just Multiply
strToInput "negate" = Just Negate
strToInput "discard" = Just Discard
strToInput x = if all isDigit (unpack x)
               then Just (Number (cast x))
               else Nothing

doUnaryOp : (Integer -> Integer) -> StackCmd () (S height) (S height)
doUnaryOp op = do val <- Pop
                  Push (op val)

doBinaryOp : (Integer -> Integer -> Integer) -> StackCmd () (S (S height)) (S height)
doBinaryOp op = do val1 <- Pop
                   val2 <- Pop
                   Push (op val2 val1)

mutual
    tryUnaryOp : (Integer -> Integer) -> StackIO height
    tryUnaryOp {height = (S h)} op = do doUnaryOp op
                                        result <- Top
                                        PutStr (show result ++ "\n")
                                        stackCalc
    tryUnaryOp _ = do PutStr "Fewer than one item on the stack\n"
                      stackCalc

    tryBinaryOp : (Integer -> Integer -> Integer) -> StackIO height
    tryBinaryOp {height = (S (S h))} op = do doBinaryOp op
                                             result <- Top
                                             PutStr (show result ++ "\n")
                                             stackCalc
    tryBinaryOp _ = do PutStr "Fewer than two items on the stack\n"
                       stackCalc

    tryDiscard : StackIO height
    tryDiscard {height = (S h)} = do val <- Pop
                                     PutStr ("Discarded " ++ show val ++ "\n")
                                     stackCalc
    tryDiscard = do PutStr "Nothing to discard\n"
                    stackCalc

    stackCalc : StackIO height
    stackCalc = do PutStr "> "
                   input <- GetStr
                   case strToInput input of
                       Nothing => do PutStr "Invalid input\n"
                                     stackCalc
                       Just (Number x) => do Push x
                                             stackCalc
                       Just Add => tryBinaryOp (+)
                       Just Subtract => tryBinaryOp (-)
                       Just Multiply => tryBinaryOp (*)
                       Just Negate => tryUnaryOp negate
                       Just Discard => tryDiscard

partial main : IO ()
main = run forever [] stackCalc
