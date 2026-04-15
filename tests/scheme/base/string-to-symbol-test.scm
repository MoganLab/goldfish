(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string->symbol
;; 将字符串转换为对应的符号
;;
;; 语法
;; ----
;; (string->symbol string)
;;
;; 参数
;; ----
;; string : string?
;; 要转换的字符串。可以是空字符串、包含数字、特殊字符的任何字符串内容。
;;
;; 返回值
;; -----
;; symbol?
;; 字符串对应的符号标识符。
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是字符串类型，抛出错误。
;; wrong-number-of-args
;; 如果没有参数或参数数量超过一个，抛出错误。
;;
;; 行为特性
;; --------
;; 1. 名称转换：字符串内容会原样转换为符号标识符，保持大小写敏感
;; 2. 数字处理：数字字符串（如"123"）转换为数字名称符号，而非数值123
;; 3. 重入一致性：相同字符串多次转换返回同一个符号对象
;; 基本转换测试
(check (string->symbol "MathAgape") => 'MathAgape)
(check (string->symbol "hello") => 'hello)
(check (string->symbol "scheme-prog") => 'scheme-prog)
;; 特殊字符转换测试
(check (string->symbol "+") => '+)
(check (string->symbol "-") => '-)
(check (string->symbol "*") => '*)
(check (string->symbol "lambda") => 'lambda)
;; 大小写敏感测试
(check (string->symbol "ABC") => 'ABC)
(check (string->symbol "abc") => 'abc)
;; 数字符号化处理（重要区别）
(check-false (equal? (string->symbol "123") 123))
;; 混合字符测试
(check (string->symbol "123abc")
  =>
  (string->symbol "123abc")
) ;check
(check (string->symbol "symbol-with-dash")
  =>
  (string->symbol "symbol-with-dash")
) ;check
(check (string->symbol "symbol_with_underscore")
  =>
  (string->symbol "symbol_with_underscore")
) ;check
;; 错误处理测试
(check-catch 'wrong-type-arg (string->symbol 123))
(check-catch 'wrong-type-arg (string->symbol 'symbol))
(check-catch 'wrong-number-of-args (string->symbol "a" "b"))
(check-catch 'wrong-number-of-args (string->symbol))
;; 保留字符号化
(check (string->symbol "if") => 'if)
(check (string->symbol "define") => 'define)
(check (string->symbol "let") => 'let)
;; 保持原始往返测试
(check (string->symbol (symbol->string 'MathAgape))
  =>
  'MathAgape
) ;check
(check-report)