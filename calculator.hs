import Data.Char

data Value = INT Integer | REAL Double deriving(Show, Eq)

data Token = 
      Number Value
    | Add | Sub | Mul | Div
    | Lpar | Rpar
    | Semic
    | EOF
    | Others Char
    deriving(Show, Eq)

type Lexer = (Token, String) -- 文字列の先頭からTokenを抽出したものと残りのStringのペア

getToken :: String -> Lexer
getToken [] = (EOF, "")
getToken (x:xs)
  | isSpace x = getToken xs
  | isDigit x = let (s, ys@(y:_)) = span isDigit (x:xs) in -- span isDigit で連続する数字とそれ以外のリストのペアを返す
          if y == '.' || y == 'e' || y == 'E' -- 数字の次の記号が (., e, E)のいずれかならば浮動小数点数
          then case reads (x:xs) of
            []          -> error "Not number"
            [(y', ys')] -> (Number(REAL y'), ys')
          else (Number(INT (read s)), ys)
  | otherwise = 
    case x of
      '+' -> (Add, xs)
      '-' -> (Sub, xs)
      '*' -> (Mul, xs)
      '(' -> (Lpar, xs)
      ')' -> (Rpar, xs)
      ';' -> (Semic, xs)
      _   -> (Others x, xs)

lexer :: String -> ([Token], String)
lexer xs =
  let (t, ys) = getToken xs
  in case t of
    Semic -> ([Semic], ys)
    EOF   -> ([EOF], ys)
    _     -> let (ts, zs) = lexer ys
              in (t:ts, zs)

