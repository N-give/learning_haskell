main :: IO ()
main = getParts

getParts :: IO()
getParts = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)


createEmail :: String -> String -> String -> String
createEmail recipient bookTitle author = toPart' recipient ++
  bodyPart bookTitle ++
  fromPart author

toPart' :: String -> String
toPart' part = "Dear " ++ part ++ ",\n"

bodyPart :: String -> String
bodyPart title = "Thanks for buying " ++ title ++ ".\n"

fromPart :: String -> String
fromPart author = "Thanks,\n" ++ author
