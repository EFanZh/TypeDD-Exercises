data Format = Ch Format | Number Format | Dbl Format | Str Format | Lit String Format | End

total PrintfType : Format -> Type
PrintfType (Ch fmt) = (c : Char) -> PrintfType fmt
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Dbl fmt) = (f : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

total toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'c' :: chars) = Ch (toFormat chars)
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                            Lit lit chars' => Lit (strCons c lit) chars'
                            fmt => Lit (strCons c "") fmt

total printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Ch fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dbl fmt) acc = \f => printfFmt fmt (acc ++ show f)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

total printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
