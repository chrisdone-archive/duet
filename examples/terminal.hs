data Terminal a
 = GetLine (String -> Terminal a)
 | PutStrLn String (Terminal a)
 | Pure a

main =
  PutStrLn
    "Please enter your name: "
    (GetLine (\line -> PutStrLn (append "Hello, " line) (Pure 0)))
