-- main :: IO ()

-- toPart recipient = "Dear " ++ recipient ++ ",\n"
-- bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
-- fromPart author = "Thanks,\n" ++ author
-- createEmail recipient bookTitle author = 
--   toPart recipient ++
--   bodyPart bookTitle ++
--   fromPart author

-- main = do
--   print "Who is the email for?"
--   recipient <- getLine
--   print "What is the Title?"
--   title <- getLine
--   print "Who is the Author?"
--   author <- getLine
--   print (createEmail recipient title author)

-- --

-- simple x = x

-- calcChange owed given = 
--   if change > 0
--   then change
--   else 0
--   where
--     change = given - owed

-- doublePlusTwo x = doubleX + 2
--   where doubleX = x * 2

-- inc n = n + 1
-- double n = n * 2
-- square n = n ^ 2

-- calcEvenOrOdd n =
--   if even n
--   then n - 2
--   else 3 * n + 1

-- --

-- sumSquareOrSquareSum x y =
--   if sumSquare > squareSum
--   then sumSquare
--   else squareSum
--   where
--     sumSquare = x^2 + y^2
--     squareSum = (x+y)^2

-- body =
--   (
--     \sumSquare squareSum ->
--     if sumSquare > squareSum
--     then sumSquare
--     else squareSum
--   )

-- doubleDouble x = (\x -> x * 2) x * 2

-- overwrite x = (\x ->
--     (\x ->
--       (\x -> x) 4
--     ) 3
--   ) 2

-- counter x = (\x ->
--     1 + (\x -> x + 1) x
--   ) x

-- --

-- ファーストクラス関数(共通部分の抽象化)
-- ifEven myFunction x =
--   if even x
--   then myFunction x
--   else x


-- inc n = n + 1
-- double n = n * 2
-- square n = n ^ 2

-- ifEvenInc n = inc n
-- ifEvenDouble n = double n
-- ifEvenSquare n = square n

-- triple n = n * 3
-- ifEvenTriple n = triple n

-- --

-- import Data.List

-- names = 
--   [
--     ("Ian", "Curtis"),
--     ("Bernard", "Sumner"),
--     ("Peter", "Hook"),
--     ("Stephan", "Morris")
--   ]

-- compareNames name1 name2 = 
--   if lastName1 > lastName2
--     then GT
--     else if lastName1 < lastName2
--       then LT
--       else if firstName1 > firstName2
--         then GT
--         else if firstName1 < firstName2
--           then LT
--           else EQ
--   -- Tuple : 2つのアイテムなら fst(1), snd(2)
--   where
--     firstName1 = fst name1
--     firstName2 = fst name2
--     lastName1 = snd name1
--     lastName2 = snd name2

-- --

-- sfOffice name =
--   if lastName < "L"
--     then nameText
--       ++ " - PO Box 1234 - San Francisco, CA, 94111"
--     else nameText
--       ++ " - PO Box 1010 - San Francisco, CA, 94109"
--   where
--     lastName = snd name
--     nameText = (fst name) ++ " " ++ lastName

-- nyOffice name =
--   nameText ++ ": PO Box 789 - New York, NY, 10013"
--   where
--     nameText = (fst name) ++ " " ++ (snd name)

-- renoOffice name =
--   nameText ++ " - PO Box 456 - Reno, NV 89523"
--   where
--     nameText = snd name

-- wdcOffice name = 
--   nameText ++ " - PO Box 234 - Washington, DC 77777"
--   where
--     nameText = (fst name) ++ " " ++ (snd name) ++ " Esq"

-- getLocationFunction location =
--   case location of -- location の値を調べる case 式、switch みたいなもの
--     "ny" -> nyOffice
--     "sf" -> sfOffice
--     "reno" -> renoOffice
--     "wdc" -> wdcOffice
--     _ -> (\name -> (fst name) ++ " " ++ (snd name))

-- addressLetter name location = locationFunction name
--   where locationFunction = getLocationFunction location

-- --

-- names = 
--   [
--     ("Ian", "Curtis"),
--     ("Bernard", "Sumner"),
--     ("Peter", "Hook"),
--     ("Stephan", "Morris")
--   ]

-- compareLastNames name1 name2 = compare lastName1 lastName2
--   where
--     lastName1 = snd name1
--     lastName2 = snd name2

-- -- 

-- inc x = x + 1

-- ifEven f x =
--   if even x
--     then f x
--     else x


-- getIfXEven x = (\f -> ifEven f x)

-- getRequestUrl host apiKey resource id =
--   host ++
--   "/" ++
--   resource ++
--   "/" ++
--   id ++
--   "?token=" ++
--   apiKey

-- getHostRequestBuilder host =
--   (
--     \apiKey resource id ->
--       getRequestUrl host apiKey resource id
--   )

-- exampleUrlBuilder = getHostRequestBuilder "http://example.com"

-- getApiRequestBuilder hostBuilder apiKey resource = 
--   (
--     \id ->
--       hostBuilder apiKey resource id
--   )

-- myExampleUrlBuilder = getApiRequestBuilder exampleUrlBuilder "1337hAsk311" "book"

-- subtract2 = flip (-) 2 -- flip は 3 つ引数受け取れる、2つ渡してるのが subtract2 なので残り1つを受け付ける

-- ifEven f x =
--   if even x
--     then f x
--     else x

-- inc n = n + 1
-- double n = n * 2
-- square n = n ^ 2

-- ifEvenInc = ifEven inc
-- ifEvenDouble = ifEven double
-- ifEvenSquare = ifEven square

-- binaryPartialApplication f arg = (\x -> f arg x)

-- respond phrase =
--   if '!' `elem` phrase
--     then "wow!"
--     else "uh... okay"

-- takeLast n aList = reverse (take n (reverse aList))

-- myRepeat n = cycle [n]

-- subseq start end aList = drop start (take end aList)

inFirstHalf e aList = e `elem` firstHalf
  where
    midpoint = (length aList) `div` 2
    firstHalf = take midpoint aList

