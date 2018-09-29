%default total

data Matter = Solid | Liquid | Gas

data MatterCmd : Type -> Matter -> Matter -> Type where
    Melt : MatterCmd () Solid Liquid
    Freeze : MatterCmd () Liquid Solid
    Boil : MatterCmd () Liquid Gas
    Condense : MatterCmd () Gas Liquid
    (>>=) : MatterCmd a state1 state2 -> (a -> MatterCmd b state2 state3) -> MatterCmd b state1 state3

iceSteam : MatterCmd () Solid Gas
iceSteam = do Melt
              Boil

steamIce : MatterCmd () Gas Solid
steamIce = do Condense
              Freeze

-- overMelt : MatterCmd () Solid Gas
-- overMelt = do Melt
--               Melt
