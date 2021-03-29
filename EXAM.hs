data Mystery
  = Many Mystery Char Mystery
  | One Char
  | Zero

toString :: Mystery -> String
toString Zero = "Zero"
toString (One c) = [c]
toString (Many m1 c m2) = toString m1 ++ [c] ++ toString m2

main :: IO ()
main = print ()