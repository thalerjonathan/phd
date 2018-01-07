data Format = Number Format
            | Str Format
            | Chr Format
            | Dbl Format
            | Lit String Format
            | End

PrintfType : Format -> Type            
PrintfType (Number fmt)  = (i : Int)    -> PrintfType fmt
PrintfType (Str fmt)     = (s : String) -> PrintfType fmt
PrintfType (Chr fmt)     = (c : Char)   -> PrintfType fmt
PrintfType (Dbl fmt)     = (d : Double) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End           = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc  = \i   => printfFmt fmt (acc ++ show i) 
printfFmt (Str fmt) acc     = \str => printfFmt fmt (acc ++ str) 
printfFmt (Chr fmt) acc     = \c   => printfFmt fmt (acc ++ (cast c)) 
printfFmt (Dbl fmt) acc     = \d   => printfFmt fmt (acc ++ show d)
printfFmt (Lit lit fmt) acc =          printfFmt fmt (acc ++ lit)
printfFmt End acc           = acc

toFormat : (xs : List Char) -> Format
toFormat []                    = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars)        = Lit "%" (toFormat chars)
toFormat (c :: chars)          = case toFormat chars of
                                      Lit lit chars' => Lit (strCons c lit) chars'
                                      fmt            => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""