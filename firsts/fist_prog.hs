main = do
  getParts

getParts :: IO()
getParts = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)


createEmail recipient bookTitle author = toPart recipient ++
  bodyPart bookTitle ++
  fromPart author

toPart recip = "Dear " ++ recip ++ ",\n"

bodyPart title = "Thanks for buying " ++ title ++ ".\n"

fromPart author = "Thanks,\n" ++ author
