data Last : List a -> a -> Type where
    LastOne : Last [value] value
    LastCons : (prf : Last xs value) -> Last (x :: xs) value

total notLastInEmpty : Last [] x -> Void
notLastInEmpty LastOne impossible
notLastInEmpty (LastCons _) impossible

total notTheOnlyOne : ((x = value) -> Void) -> Last [x] value -> Void
notTheOnlyOne contra LastOne = contra Refl
notTheOnlyOne contra (LastCons prf) impossible

total notLastInTail : (Last (y :: xs) value -> Void) -> Last (x :: (y :: xs)) value -> Void
notLastInTail contra (LastCons prf) = contra prf

total isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastInEmpty
isLast (x :: []) value = case decEq x value of
                             No contra => No (notTheOnlyOne contra)
                             Yes Refl => Yes LastOne
isLast (x :: (y :: xs)) value = case isLast (y :: xs) value of
                                    No contra => No (notLastInTail contra)
                                    Yes prf => Yes (LastCons prf)
