module Gen.Utils where

eraseRight :: Either a b -> Either a ()
eraseRight (Right _) = Right ()
eraseRight (Left l)  = Left l

eraseLeft :: Either a b -> Either () b
eraseLeft (Right l) = Right l
eraseLeft (Left _)  = Left ()

eraseBoth :: Either a b -> Either () ()
eraseBoth (Right _) = Right ()
eraseBoth (Left _)  = Left ()