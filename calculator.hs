import Data.Char
-- import Control.Monad.Instances
import System.IO
-- import Data.Monad

-- 値
data Value = INT Integer | REAL Double deriving(Show, Eq)

-- トークン
data Token = 
    Number Value  -- ex. (INT 10), (REAL 103.4)
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

-- 字句解析
lexer :: String -> ([Token], String)
lexer xs =
  let (t, ys) = getToken xs
  in case t of
    Semic -> ([Semic], ys)
    EOF   -> ([EOF], ys)
    _     -> let (ts, zs) = lexer ys
              in (t:ts, zs)


-- 構文解析              
-- 構文木
data Expr = Num Value
  | Op1 (Value -> Value) Expr -- 単項演算 ex. -1
  | Op2 (Value -> Value -> Value) Expr Expr　-- 二項演算 ex. (1 * 3) + (1 - (3 / 1))

type Calc a = Either String a -- Monadわからない

-- エラー処理
calcError :: String -> Calc a
calcError s = Left s

-- 数式の計算
-- 式がexpr, 項がterm, 因子がfactor 正常に解析ができたときは Right (Expr, [Token]), 式に間違いがあれば Left Stringを返す

-- リストの先頭トークンがNumberであれば(Num x, xs)を返す. return でEitherモナドに包む
-- '('であればexprを')'が来るで再帰処理して式eを求めEitherモナドに包んで(e, ys)を返す. ')'がなければcalcErrorでLeft Stringを返す
-- トークンがSubであればexprを再帰呼び出しして値をOp1にセットして返す. negは単項演算子 - を処理する関数
factor :: [Token] -> Calc (Expr, [Token])
factor (Number x : xs) = return (Num x, xs) -- 先頭がNumberだったときの処理
factor (Lpar: xs) = expr xs >>= \(e, y:ys) ->  -- >>= は連結を表す(おそらくexpr xs の返り値をラムダ計算の引数に入力するという意味だと思う)
  case y of
    Rpar -> return (e, ys)
    _    -> calcError "')' expected"
factor (Sub:xs) = expr xs >>= \(e, ys) -> return (Op1 neg e, ys) 
factor (Add:xs) = expr xs
factor (EOF:xs) = calcError "End of File"
factor (x:_)    = calcError ("unexpected Token" ++ show x)

-- 最初にfactorを呼び出し, 因子の値を局所変数term_subに渡す
-- トークンがMul, Divの場合factorを呼び出して次の因子を求めOp2に格納してterm_subを再帰呼び出し.
-- トークンがMul, Div以外の場合zsをEitherモナドに包んで返す
term :: [Token] -> Calc (Expr, [Token])
term xs = factor xs >>= term_sub
  where
    opList = [(Mul, mul), (Div, div')]
    term_sub zs@(e, y:ys) = 
      case lookup y opList of
        Nothing -> return zs
        Just op -> factor ys >>= \(e', ys') -> term_sub (Op2 op e e', ys')


-- term のAdd, Sub版
expr :: [Token] -> Calc (Expr, [Token])
expr xs = term xs >>= expr_sub
  where
    opList = [(Add, add), (Sub, sub)]
    expr_sub zs@(e, y:ys) = 
      case lookup y opList of
        Nothing -> return zs
        Just op -> term ys >>= \(e', ys') -> expr_sub (Op2 op e e', ys')

-- exprを呼び出す関数
expression ::[Token] -> Calc (Expr, [Token])
expression xs = expr xs >>= \(e, y:ys) ->
  case y of
    Semic -> return (e, ys)
    _     -> calcError "expression error"

-- 構文木の評価
evalExpr :: Expr -> Value
evalExpr (Num x) = x
evalExpr (Op1 op e) = op (evalExpr e)
evalExpr (Op2 op e1 e2) = op (evalExpr e1) (evalExpr e2)

-- 算術演算
neg :: Value -> Value
neg (INT x) = INT (- x)
neg (REAL x) = REAL (- x)

add :: Value -> Value -> Value
add (INT x) (INT y) = INT (x + y)
add (REAL x) (REAL y) = REAL (x + y)
add (INT x)  (REAL y) = REAL (fromIntegral x + y)
add (REAL x) (INT y)  = REAL (x + fromIntegral y)

sub :: Value -> Value -> Value
sub (INT x) (INT y) = INT (x - y)
sub (REAL x) (REAL y) = REAL (x - y)
sub (INT x)  (REAL y) = REAL (fromIntegral x - y)
sub (REAL x) (INT y)  = REAL (x - fromIntegral y)

mul :: Value -> Value -> Value
mul (INT x) (INT y) = INT (x * y)
mul (REAL x) (REAL y) = REAL (x * y)
mul (INT x)  (REAL y) = REAL (fromIntegral x * y)
mul (REAL x) (INT y)  = REAL (x * fromIntegral y)

div' :: Value -> Value -> Value
div' (INT x) (INT y) = INT (x `div` y)
div' (REAL x) (REAL y) = REAL (x / y)
div' (INT x)  (REAL y) = REAL (fromIntegral x / y)
div' (REAL x) (INT y)  = REAL (x / fromIntegral y)


-- 式の入力と評価
topLevel :: String -> IO ()
topLevel xs = do
  putStr "Calc>> "
  -- 字句解析
  let (ys, xs') = lexer xs
  -- 構文解析
  case expression ys of
    Left mes -> do
      putStrLn mes
      topLevel xs'
    Right (e, _) -> do 
      case evalExpr e of 
        INT x -> putStrLn $ show x
        REAL x -> putStrLn $ show x
      topLevel xs'

main :: IO ()
main = do
  xs <- hGetContents stdin
  topLevel xs