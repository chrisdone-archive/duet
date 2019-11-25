data Tuple a b = Tuple a b
data Result a = OK a String | Error String
data Parser a = Parser (String -> Result a)
parseBool =
  Parser
    (\string ->
       case take 4 string of
         "True" ->
           case drop 4 string of
             !rest -> OK True rest
         _ ->
           case take 5 string of
             "False" ->
               case drop 5 string of
                 !rest -> OK False rest
             _ -> Error (append "Expected a bool, but got: " string))
runParser =
  \p s ->
    case p of
      Parser f -> f s
bind =
  \m f ->
    Parser
      (\s ->
         case runParser m s of
           OK a rest -> runParser (f a) rest
           Error err -> Error err)
pure = \a -> Parser (OK a)
main = runParser (bind parseBool (\x -> bind parseBool (\y -> pure (Tuple x y)))) "TrueFalse"
