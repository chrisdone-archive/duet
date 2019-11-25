data Result a = OK a String | Error String
data Parser a = Parser (String -> Result a)
parseBool =
  Parser
    (\string ->
       case take 4 string of
         "True" -> OK True (drop 4 string)
         _ ->
           case take 5 string of
             "False" -> OK False (drop 5 string)
             _ -> Error (append "Expected a bool, but got: " string))
runParser =
  \p s ->
    case p of
      Parser f -> f s
main = runParser parseBool "TrueFalse"
