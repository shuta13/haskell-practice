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


import Data.List

names = 
  [
    ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephan", "Morris")
  ]

compareLastNames name1 name2 = 
  if lastName1 > lastName2
    then GT
    else if lastName1 < lastName2
      then LT
      else if firstName1 > firstName2
        then GT
        else if firstName1 < firstName2
          then LT
          else EQ
  -- Tuple : 2つのアイテムなら fst(1), snd(2)
  where
    firstName1 = fst name1
    firstName2 = fst name2
    lastName1 = snd name1
    lastName2 = snd name2

