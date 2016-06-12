data Format = Number Format
            | Str Format
            | Ch Format
            | Doub Format
            | Lit String Format
            | End
%name Format fmt, fmt1, fmt2

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Ch fmt) = (ch : Char) -> PrintfType fmt
PrintfType (Doub fmt) = (d : Double) -> PrintfType fmt
PrintfType (Lit x fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Ch fmt) acc = \c => printfFmt fmt (acc ++ cast c)
printfFmt (Doub fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Lit x fmt) acc = printfFmt fmt (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat ('%' :: 'c' :: xs) = Ch (toFormat xs)
toFormat ('%' :: 'f' :: xs) = Doub (toFormat xs)
toFormat ('%' :: xs) = Lit "%" (toFormat xs)
toFormat (c :: chars) = case toFormat chars of
                             (Lit lit chars') => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""
