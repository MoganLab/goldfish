(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; symbol->string
;; 将符号转换为字符串形式
;;
;; 语法
;; ----
;; (symbol->string symbol)
;;
;; 参数
;; ----
;; symbol : symbol?
;; 要转换的符号
;;
;; 返回值
;; -----
;; string?
;; 符号对应的字符串表示
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是符号类型，抛出错误。
;;
;; 说明
;; ----
;; symbol->string将符号的标识符转换为等效的字符串表示。
;; 注意区分大小写：符号'abc和'ABC会转换为"abc"和"ABC"的不同字符串。
;; 基本测试
(check (symbol->string 'MathAgape) => "MathAgape")
(check (symbol->string 'goldfish-scheme)
  =>
  "goldfish-scheme"
) ;check
(check (symbol->string (string->symbol "Hello World"))
  =>
  "Hello World"
) ;check
;; 特殊符号测试
(check (symbol->string '+) => "+")
(check (symbol->string '-) => "-")
(check (symbol->string '*) => "*")
(check (symbol->string '/) => "/")
(check (symbol->string '=>) => "=>")
(check (symbol->string '<=) => "<=")
;; 大小写敏感测试
(check (symbol->string 'ABC) => "ABC")
(check (symbol->string 'abc) => "abc")
(check (symbol->string 'LispCase) => "LispCase")
(check (symbol->string 'camelCase) => "camelCase")
;; 边界测试
(check (symbol->string 'a) => "a")
(check (symbol->string 'x) => "x")
(check (symbol->string 'empty) => "empty")
;; 数字和特殊字符测试
(check (symbol->string (string->symbol "123")) => "123")
(check (symbol->string (string->symbol "123abc"))
  =>
  "123abc"
) ;check
(check (symbol->string (string->symbol "symbol_with_underscore"))
  =>
  "symbol_with_underscore"
) ;check
(check (symbol->string (string->symbol "symbol-with-dash"))
  =>
  "symbol-with-dash"
) ;check
(check (symbol->string (string->symbol "sym$bol"))
  =>
  "sym$bol"
) ;check
;; 错误测试
(check-catch 'wrong-type-arg (symbol->string 123))
(check-catch 'wrong-type-arg (symbol->string "symbol"))
(check-catch 'wrong-type-arg (symbol->string #f))
(check-catch 'wrong-type-arg (symbol->string '()))
(check-catch 'wrong-number-of-args (symbol->string 'a 'b))
(check-catch 'wrong-number-of-args (symbol->string))
;; 往返转换测试
(let ((test-symbols '(hello world scheme-prog example complex-identifier my-symbol)
      ) ;test-symbols
     ) ;
  (for-each (lambda (sym)
              (let ((str (symbol->string sym)))
                (check (string->symbol str) => sym)
              ) ;let
            ) ;lambda
    test-symbols
  ) ;for-each
) ;let
(check-report)