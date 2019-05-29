import Text.Regex.Posix

main = putStrLn (tokenizer "1.2")


tokenizer :: String -> String
tokenizer arg
  | arg =~ "^[A-Z]" = "KEYWORD"
  | arg =~ "\\d+" = "INTLITERAL"
  | arg =~ "\\d*\\.\\d+" = "FLOATLITERAL"
  | arg =~ "\\w{1}[\\w\\d]*" = "IDENTIFIER"
  | otherwise = "Not testing anything else"
