import Text.Parsec
import Text.Parsec.Combinator

data IP = IP Int Int Int Int deriving (Show)

number :: Parsec String u Int
number = many1 digit >>= (return . read)

dot = char '.'

parseIP = do
  p1 <- number
  dot
  p2 <- number
  dot
  p3 <- number
  dot
  p4 <- number
  return $ IP p1 p2 p3 p4