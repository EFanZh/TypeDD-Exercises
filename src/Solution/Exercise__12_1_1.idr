import Control.Monad.State

%default total

update : (stateType -> stateType) -> State stateType ()
update f = do x <- get
              put (f x)
