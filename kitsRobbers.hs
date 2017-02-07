kitsRobbers :: Integer -> String -> String
kitsRobbers 0 ys = ys
kitsRobbers x ys = kitsRobbers (x-1) (addVowels ys)

addVowels :: String -> String
addVowels (y:[]) = addVowel y
addVowels (y:ys) = (addVowel y) ++ (addVowels ys)

addVowel :: Char -> String
addVowel x = if elem x "bcdfghjklmnpqrstvxz"
              then x:'o':x:[]
              else x:[]
