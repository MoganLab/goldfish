(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; string-set!
;; 修改字符串中指定位置的字符，返回修改后的字符串。在R7RS标准中，string-set!是一个立即执行的变异操作，不会创建新的字符串对象。
;;
;; 语法
;; ----
;; (string-set! string k char)
;;
;; 参数
;; ----
;; string : string?
;; 要修改的原始字符串。必须是非常量字符串。
;;
;; k : exact?
;; 必须是非负的精确整数，表示要修改的字符索引位置。必须小于字符串长度。
;;
;; char : char?
;; 新的字符值，用于替换位置k处的原始字符。
;;
;; 返回值
;; ------
;; unspecified
;; 按照R7RS规范，返回未指定的值。
;;
;; 说明
;; ----
;; 1. 这是一个变异操作，会直接修改原始字符串对象的内容
;; 2. 索引k是从0开始计算的
;; 3. 可以用来修改任何位置的合法字符，但不能用于扩展字符串长度
;; 4. 修改后的字符串与新字符串不同的引用指向相同的内存内容
;; 5. 参数必须是变量引用或动态创建的字符串，不能是字符串常量
;;
;; 错误处理
;; --------
;; out-of-range
;; 当索引k为负数或大于等于字符串长度时抛出错误。
;;
;; wrong-type-arg
;; 当string不是字符串、k不是精确整数、char不是字符时抛出错误。

;; string-set! 基础测试
(let ((str (string-copy "hello")))
  (string-set! str 1 #\A)
  (check str => "hAllo")
) ;let

(let ((str (string-copy "abc")))
  (string-set! str 0 #\X)
  (string-set! str 2 #\Z)
  (check str => "XbZ")
) ;let

;; 修改不同位置测试
(let ((str (string-copy "123456")))
  (string-set! str 0 #\0)
  (string-set! str 5 #\9)
  (check str => "023459")
) ;let

;; 边界位置测试
(let ((str (string-copy "a")))
  (string-set! str 0 #\A)
  (check str => "A")
) ;let

(let ((str (string-copy "xyz")))
  (string-set! str 0 #\1)
  (string-set! str 1 #\2)
  (string-set! str 2 #\3)
  (check str => "123")
) ;let

;; 特殊字符测试
(let ((str (string-copy "hello world")))
  (string-set! str 5 #\-)
  (check str => "hello-world")
) ;let

(let ((str (string-copy "Test!")))
  (string-set! str 4 #\?)
  (check str => "Test?")
) ;let

;; 数字字符串测试
(let ((str (string-copy "00000")))
  (string-set! str 2 #\1)
  (check str => "00100")
) ;let

;; 连续多次修改
(let ((str (string-copy "original")))
  (string-set! str 0 #\O)
  (string-set! str 1 #\R)
  (string-set! str 2 #\I)
  (string-set! str 3 #\G)
  (string-set! str 4 #\I)
  (string-set! str 5 #\N)
  (string-set! str 6 #\A)
  (string-set! str 7 #\L)
  (check str => "ORIGINAL")
) ;let

;; 测试索引在有效范围内
(let ((str (string-copy "test")))
  (string-set! str 0 #\T)
  (string-set! str 1 #\E)
  (string-set! str 2 #\S)
  (string-set! str 3 #\T)
  (check str => "TEST")
) ;let

;; 错误处理测试
;; 索引越界测试
(let ((str (string-copy "abc")))
  (check-catch 'out-of-range (string-set! str -1 #\x))
  (check-catch 'out-of-range (string-set! str 3 #\x))
) ;let

(let ((str (string-copy "")))
  (check-catch 'out-of-range (string-set! str 0 #\x))
) ;let

;; 类型错误测试
(check-catch 'wrong-type-arg (string-set! 123 0 #\A))
(check-catch 'wrong-type-arg (string-set! "hello" 0.5 #\A))
(check-catch 'wrong-type-arg (string-set! "hello" 0 123))
(check-catch 'wrong-type-arg (string-set! "hello" 1 "A"))

;; 参数数量错误测试
(check-catch 'wrong-number-of-args (string-set!))
(check-catch 'wrong-number-of-args (string-set! "hello"))
(check-catch 'wrong-number-of-args (string-set! "hello" 1))
(check-catch 'wrong-number-of-args (string-set! "hello" 1 #\a #\b))

;; 变量引用一致性测试
(let ((str1 (string-copy "hello")))
  (let ((str2 str1))
    (string-set! str1 1 #\E)
    (check str1 => "hEllo")
    (check str2 => "hEllo")
  ) ;let
) ;let

;; 与string-ref结合使用测试
(let ((str (string-copy "test")))
  (check (string-ref str 0) => #\t)
  (string-set! str 0 #\T)
  (check (string-ref str 0) => #\T)
  (check str => "Test")
) ;let

;; 复杂字符串修改场景测试
(let ((str (string-copy "programming")))
  (string-set! str 0 #\P)
  (string-set! str 8 #\N)
  (string-set! str 10 #\G)
  (check (string-ref str 0) => #\P)
  (check (string-ref str 8) => #\N)
  (check (string-ref str 10) => #\G)
) ;let

(check-report)
