# Calculator with Haskell

- Haskell勉強用
- コンパイラの流れを理解する用

## 処理の流れ
- 字句解析　-> 構文解析 -> 意味解析


- lexer にて字句解析
入力された文字列からトークンへ変換
  - 13 -> (Num (INT 13))
  - 1.34 -> (Num (REAL 1.34))
  - '+' -> Add
などのリストのリストへ変換する

-expression にて構文解析
expr -> term -> factor で再帰降下法を行う

- evalExpr で意味解析

## 参考
http://www.geocities.co.jp/SiliconValley-Oakland/1680/func/haskell27.html
