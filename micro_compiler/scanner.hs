import Text.Regex.Posix

main = do
  putStrLn (tokenizer "1.2")


tokenizer :: String -> String
tokenizer arg
  | arg =~ "^[A-Z]" = "KEYWORD"
  | arg =~ "\\w{1}[\\w\\d]*" = "IDENTIFIER"
  | arg =~ "\\d+" = "INTLITERAL"
  | arg =~ "\\d*\\.\\d+" = "FLOATLITERAL"
  | otherwise = "Not testing anything else"
