(import (liii check)
        (liii string)
) ;import

;; string-any
;; 检查字符串中的任意字符是否满足给定的条件。
;;
;; 语法
;; ----
;; (string-any char/pred? str)
;; (string-any char/pred? str start)
;; (string-any char/pred? str start end)
;;
;; 参数
;; ----
;; char/pred? : char 或 procedure?
;; - 字符(char)：检查字符串中是否存在与该字符相等的字符
;; - 谓词(procedure)：接受单个字符作为参数，返回布尔值
;;
;; str : string?
;; 要检查的字符串
;;
;; start : integer? 可选
;; 检查的起始位置(包含)，默认为0
;;
;; end : integer? 可选
;; 检查的结束位置(不包含)，默认为字符串长度
;;
;; 返回值
;; ----
;; boolean
;; - 如果字符串中至少有一个字符满足条件则返回#t，否则返回#f
;; - 对于空字符串或空范围始终返回#f
;;
;; 注意
;; ----
;; string-any是string-every的对偶函数。与检查每个字符是否满足条件的string-every不同，string-any只需要找到至少一个满足条件的字符即可返回真值。
;; 该函数也支持start和end参数来限定检查范围。
;; 空字符串或空范围会返回#f，因为没有任何字符满足条件。
;;
;; 示例
;; ----
;; (string-any char-numeric? "abc123") => #t
;; (string-any char-numeric? "hello") => #f
;; (string-any char-alphabetic? "12345a") => #t
;; (string-any char-alphabetic? "12345") => #f
;; (string-any char-upper-case? "abC12") => #t
;; (string-any char-whitespace? "hello") => #f
;; (string-any #\a "zebra") => #\a
;; (string-any #\z "apple") => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串时

; Basic functionality tests for character parameter
(check-true (string-any #\a "abcde"))
(check-false (string-any #\z "abcde"))
(check-false (string-any #\a "xyz"))
(check-true (string-any #\x "abcxdef"))

; Basic functionality tests for predicate parameter
(check-true (string-any char-numeric? "abc123"))
(check-false (string-any char-numeric? "hello"))
(check-true (string-any char-alphabetic? "12345a"))
(check-false (string-any char-alphabetic? "12345"))
(check-true (string-any char-upper-case? "hello World"))
(check-false (string-any char-upper-case? "hello world"))

; Empty string handling
(check-false (string-any #\a ""))
(check-false (string-any char-numeric? ""))

; Single character strings
(check-true (string-any #\a "a"))
(check-false (string-any #\b "a"))
(check-true (string-any char-numeric? "1"))
(check-false (string-any char-numeric? "a"))

; Whitespace and special characters
(check-true (string-any char-whitespace? "hello world"))
(check-false (string-any char-whitespace? "hello"))
(check-true (string-any (lambda (c) (char=? c #\h)) "hello"))
(check-true (string-any (lambda (c) (char=? c #\!)) "hello!"))

; Complex character tests
(check-true (string-any char-alphabetic? "HELLO"))
(check-true (string-any char-numeric? "123abc"))

; Original legacy tests
(check-true (string-any #\0 "xxx0xx"))
(check-false (string-any #\0 "xxxxxx"))
(check-true (string-any char-numeric? "xxx0xx"))
(check-false (string-any char-numeric? "xxxxxx"))

; Start/end parameter tests
(check-true (string-any char-alphabetic? "01c345" 2))
(check-false (string-any char-alphabetic? "01c345" 3))
(check-true (string-any char-alphabetic? "01c345" 2 4))
(check-false (string-any char-alphabetic? "01c345" 2 2))
(check-false (string-any char-alphabetic? "01c345" 3 4))
(check-true (string-any char-alphabetic? "01c345" 2 6))

; Additional comprehensive tests for start/end parameters
(check-true (string-any #\a "012a34" 0))
(check-false (string-any #\a "012345" 0 2))
(check-true (string-any #\0 "012345" 0 1))
(check-false (string-any #\a "bbbccc" 1 3))
(check-true (string-any char-alphabetic? "1a23bc" 1 4))
(check-false (string-any char-alphabetic? "123456" 0 3))

; Edge cases
(check-true (string-any char-alphabetic? "abc" 0 3))
(check-false (string-any char-alphabetic? "123" 0 3))
(check-true (string-any #\a "aab" 1 2))
(check-false (string-any #\a "bbc" 1 2))
(check-true (string-any char-alphabetic? "a" 0 1))
(check-false (string-any char-alphabetic? "" 0 0))

; Custom predicate tests
(check-true (string-any (lambda (c) (char=? c #\x)) "hello x there"))
(check-false (string-any (lambda (c) (char=? c #\z)) "hello w there"))
(check-true (string-any char-alphabetic? "HELLO"))
(check-true (string-any char-alphabetic? "123a"))

(check
  (catch 'out-of-range
    (lambda ()
      (string-any
        char-alphabetic?
        "01c345"
        2
        7
      ) ;string-any
    ) ;lambda
    (lambda args #t)
  ) ;catch
  =>
  #t
) ;check

(check
  (catch 'out-of-range
    (lambda ()
      (string-any
        char-alphabetic?
        "01c345"
        2
        1
      ) ;string-any
    ) ;lambda
    (lambda args #t)
  ) ;catch
  =>
  #t
) ;check

; Error handling tests for string-any
(check-catch 'wrong-type-arg (string-any 123 "hello"))
(check-catch 'wrong-type-arg (string-any "a" "hello"))
(check-catch 'wrong-type-arg (string-any '(a b) "hello"))
(check-catch 'wrong-type-arg (string-any (lambda (n) (= n 0)) "hello"))
(check-catch 'wrong-type-arg (string-any char-alphabetic? 123))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" "0"))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" 1.5))
(check-catch 'wrong-type-arg (string-any char-alphabetic? "hello" 'a))

; Out of range tests
(check-catch 'out-of-range (string-any char-alphabetic? "hello" -1))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 0 6))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 5 1))
(check-catch 'out-of-range (string-any char-alphabetic? "hello" 10))

;; === string-any多字节字符边界验证增强 ===
;; 中文和ASCII字符混用验证：确保ASCII和中文混合文本与谓词函数的边界行为一致性
(check-true (string-any char-alphabetic? "a中文b"))          ; 中英文混合必须匹配英文字母字符
(check-true (string-any char-alphabetic? "hello中文"))      ; ASCII字母+中文混合中字母存在
(check-true (string-any char-numeric? "中文123文字"))       ; 中文+数字混合中数字存在
(check-false (string-any char-numeric? "中文测试"))         ; 中文文本中不含数字，返回#f

;; 中文字符基础行为验证：确保谓词对Unicode中文字符处理无异常
(check-true (string-any (lambda (c) #t) "中文文档"))        ; 中文字符全匹配任意谓词
(check-true (string-any (lambda (c) (char=? c #\a)) "中文a文字"))   ; 特定ASCII字符在混合文本中匹配
(check-false (string-any (lambda (c) (char=? c #\z)) "中文测试"))   ; 不存在的字符匹配验证

;; emoji字符边界验证：确保4字节emoji在UTF-8编码环境中的字节级处理正确性
(check-true (string-any char-numeric? "123😀456"))         ; emoji混在数字中，确保数字字符被识别
(check-true (string-any char-alphabetic? "hello😀world"))   ; emoji混在字母中，字母字符存在
(check-true (string-any (lambda (c) (not (char-whitespace? c))) "hello 😀world"))   ; 空白符+文字+emoji混合
(check-false (string-any char-alphabetic? "123😀!@#"))      ; 数字+emoji+符号组合无字母字符

;; 扩展Unicode字符验证：涵盖特殊符号、数学符号等扩展应用场景
(check-true (string-any char-numeric? "￥1000"))           ; ￥货币符号+数字组合的数字存在
(check-true (string-any char-alphabetic? "数学+a+b=c"))                   ; 数学符号+字母混合字母存在
(check-true (string-any (lambda (c) (not (char-whitespace? c))) "空格123文字😀test")) ; 空白+文字+数字非空白检测

;; 多字节字符分割边界验证：检查start/end参数在跨越多字节字符时的边界处理完整性
(check-true (string-any char-alphabetic? "a中文b" 0 6))     ; 跨越ASCII和中文边界检测字母
(check-true (string-any char-numeric? "文123字" 1 6))       ; 中文字符范围内数字检测
(check-false (string-any char-numeric? "中文测试" 0 8))     ; 中文字符范围内无数字检测
(check-true (string-any (lambda (c) (or (char-alphabetic? c) (char-numeric? c))) "混合a123文😀字" 0 15)) ; 综合范围检测

;; 空边界条件验证：空字符串和零长度范围在多字节字符文本中的处理边界
(check-false (string-any (lambda (c) #t) "中文" 4 4))        ; 中文字符串末尾边界检测
(check-false (string-any (lambda (c) #t) "" 0 0))           ; 空字符串边界验证

;; 混合场景压力测试：复杂Unicode字符环境下的谓词函数行为一致性验证
(check-true (string-any (lambda (c) (or (char-alphabetic? c) (char-numeric? c))) "混合text123和中文"))
(check-true (string-any char-alphabetic? "program中文test"))   ; 混合文本有字母存在
(check-false (string-any char-numeric? "纯中文text验证"))      ; 中文文本无数字验证

(check-report)
