%default total

record Score where
    constructor MkScore
    correct : Nat
    attempted : Nat

record GameState where
    constructor MkGameState
    score : Score
    difficulty : Int

Show GameState where
    show st = show (correct (score st)) ++ "/" ++
        show (attempted (score st)) ++ "\n" ++
        "Difficulty: " ++ show (difficulty st)

data Command : Type -> Type where
    PutStr : String -> Command ()
    GetLine : Command String
    GetRandom : Command Int
    GetGameState : Command GameState
    PutGameState : GameState -> Command ()
    Pure : ty -> Command ty
    Bind : Command a -> (a -> Command b) -> Command b

mutual
    Functor Command where
        map func command = do result <- command
                              pure (func result)

    Applicative Command where
        pure = Pure
        func <*> command = do f <- func
                              c <- command
                              pure (f c)

    Monad Command where
        (>>=) = Bind
