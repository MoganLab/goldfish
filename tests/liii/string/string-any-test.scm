(import (liii check) (liii string))

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

(check-true (string-any #\a "abcde"))
(check-false (string-any #\z "abcde"))
(check-false (string-any #\a "xyz"))
(check-true (string-any #\x "abcxdef"))

(check-true (string-any char-numeric? "abc123")
) ;check-true
(check-false (string-any char-numeric? "hello")
) ;check-false
(check-true (string-any char-alphabetic? "12345a")
) ;check-true
(check-false (string-any char-alphabetic? "12345")
) ;check-false
(check-true (string-any char-upper-case?
              "hello World"
            ) ;string-any
) ;check-true
(check-false (string-any char-upper-case?
               "hello world"
             ) ;string-any
) ;check-false

(check-false (string-any #\a ""))
(check-false (string-any char-numeric? "")
) ;check-false

(check-true (string-any #\a "a"))
(check-false (string-any #\b "a"))
(check-true (string-any char-numeric? "1")
) ;check-true
(check-false (string-any char-numeric? "a")
) ;check-false

(check-true (string-any char-whitespace?
              "hello world"
            ) ;string-any
) ;check-true
(check-false (string-any char-whitespace? "hello")
) ;check-false
(check-true (string-any (lambda (c) (char=? c #\h))
              "hello"
            ) ;string-any
) ;check-true
(check-true (string-any (lambda (c) (char=? c #\!))
              "hello!"
            ) ;string-any
) ;check-true

(check-true (string-any char-alphabetic? "HELLO")
) ;check-true
(check-true (string-any char-numeric? "123abc")
) ;check-true

(check-true (string-any #\0 "xxx0xx"))
(check-false (string-any #\0 "xxxxxx"))
(check-true (string-any char-numeric? "xxx0xx")
) ;check-true
(check-false (string-any char-numeric? "xxxxxx")
) ;check-false

(check-true (string-any char-alphabetic? "01c345" 2)
) ;check-true
(check-false (string-any char-alphabetic? "01c345" 3)
) ;check-false
(check-true (string-any char-alphabetic?
              "01c345"
              2
              4
            ) ;string-any
) ;check-true
(check-false (string-any char-alphabetic?
               "01c345"
               2
               2
             ) ;string-any
) ;check-false
(check-false (string-any char-alphabetic?
               "01c345"
               3
               4
             ) ;string-any
) ;check-false
(check-true (string-any char-alphabetic?
              "01c345"
              2
              6
            ) ;string-any
) ;check-true

(check-true (string-any #\a "012a34" 0))
(check-false (string-any #\a "012345" 0 2)
) ;check-false
(check-true (string-any #\0 "012345" 0 1)
) ;check-true
(check-false (string-any #\a "bbbccc" 1 3)
) ;check-false
(check-true (string-any char-alphabetic?
              "1a23bc"
              1
              4
            ) ;string-any
) ;check-true
(check-false (string-any char-alphabetic?
               "123456"
               0
               3
             ) ;string-any
) ;check-false

(check-true (string-any char-alphabetic? "abc" 0 3)
) ;check-true
(check-false (string-any char-alphabetic? "123" 0 3)
) ;check-false
(check-true (string-any #\a "aab" 1 2))
(check-false (string-any #\a "bbc" 1 2))
(check-true (string-any char-alphabetic? "a" 0 1)
) ;check-true
(check-false (string-any char-alphabetic? "" 0 0)
) ;check-false

(check-true (string-any (lambda (c) (char=? c #\x))
              "hello x there"
            ) ;string-any
) ;check-true
(check-false (string-any (lambda (c) (char=? c #\z))
               "hello w there"
             ) ;string-any
) ;check-false
(check-true (string-any char-alphabetic? "HELLO")
) ;check-true
(check-true (string-any char-alphabetic? "123a")
) ;check-true

(check (catch 'out-of-range
         (lambda ()
           (string-any char-alphabetic?
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

(check (catch 'out-of-range
         (lambda ()
           (string-any char-alphabetic?
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

(check-catch 'wrong-type-arg
  (string-any 123 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any "a" "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any '(a b) "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any (lambda (n) (= n 0))
    "hello"
  ) ;string-any
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any char-alphabetic? 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any char-alphabetic?
    "hello"
    "0"
  ) ;string-any
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any char-alphabetic?
    "hello"
    1.5
  ) ;string-any
) ;check-catch
(check-catch 'wrong-type-arg
  (string-any char-alphabetic? "hello" 'a)
) ;check-catch

(check-catch 'out-of-range
  (string-any char-alphabetic? "hello" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-any char-alphabetic?
    "hello"
    0
    6
  ) ;string-any
) ;check-catch
(check-catch 'out-of-range
  (string-any char-alphabetic?
    "hello"
    5
    1
  ) ;string-any
) ;check-catch
(check-catch 'out-of-range
  (string-any char-alphabetic? "hello" 10)
) ;check-catch

;; === string-any多字节字符边界验证增强 ===
;; 中文和ASCII字符混用验证：确保ASCII和中文混合文本与谓词函数的边界行为一致性
(check-true (string-any char-alphabetic? "a中文b")
) ;check-true
(check-true (string-any char-alphabetic?
              "hello中文"
            ) ;string-any
) ;check-true
(check-true (string-any char-numeric?
              "中文123文字"
            ) ;string-any
) ;check-true
(check-false (string-any char-numeric?
               "中文测试"
             ) ;string-any
) ;check-false

;; 中文字符基础行为验证：确保谓词对Unicode中文字符处理无异常
(check-true (string-any (lambda (c) #t)
              "中文文档"
            ) ;string-any
) ;check-true
(check-true (string-any (lambda (c) (char=? c #\a))
              "中文a文字"
            ) ;string-any
) ;check-true
(check-false (string-any (lambda (c) (char=? c #\z))
               "中文测试"
             ) ;string-any
) ;check-false

;; emoji字符边界验证：确保4字节emoji在UTF-8编码环境中的字节级处理正确性
(check-true (string-any char-numeric? "123😀456")
) ;check-true
(check-true (string-any char-alphabetic?
              "hello😀world"
            ) ;string-any
) ;check-true
(check-true (string-any (lambda (c) (not (char-whitespace? c)))
              "hello 😀world"
            ) ;string-any
) ;check-true
(check-false (string-any char-alphabetic?
               "123😀!@#"
             ) ;string-any
) ;check-false

;; 扩展Unicode字符验证：涵盖特殊符号、数学符号等扩展应用场景
(check-true (string-any char-numeric? "￥1000")
) ;check-true
(check-true (string-any char-alphabetic?
              "数学+a+b=c"
            ) ;string-any
) ;check-true
(check-true (string-any (lambda (c) (not (char-whitespace? c)))
              "空格123文字😀test"
            ) ;string-any
) ;check-true

;; 多字节字符分割边界验证：检查start/end参数在跨越多字节字符时的边界处理完整性
(check-true (string-any char-alphabetic?
              "a中文b"
              0
              6
            ) ;string-any
) ;check-true
(check-true (string-any char-numeric?
              "文123字"
              1
              6
            ) ;string-any
) ;check-true
(check-false (string-any char-numeric?
               "中文测试"
               0
               8
             ) ;string-any
) ;check-false
(check-true (string-any (lambda (c)
                          (or (char-alphabetic? c)
                            (char-numeric? c)
                          ) ;or
                        ) ;lambda
              "混合a123文😀字"
              0
              15
            ) ;string-any
) ;check-true

;; 空边界条件验证：空字符串和零长度范围在多字节字符文本中的处理边界
(check-false (string-any (lambda (c) #t)
               "中文"
               4
               4
             ) ;string-any
) ;check-false
(check-false (string-any (lambda (c) #t) "" 0 0)
) ;check-false

;; 混合场景压力测试：复杂Unicode字符环境下的谓词函数行为一致性验证
(check-true (string-any (lambda (c)
                          (or (char-alphabetic? c)
                            (char-numeric? c)
                          ) ;or
                        ) ;lambda
              "混合text123和中文"
            ) ;string-any
) ;check-true
(check-true (string-any char-alphabetic?
              "program中文test"
            ) ;string-any
) ;check-true
(check-false (string-any char-numeric?
               "纯中文text验证"
             ) ;string-any
) ;check-false

(check-report)
