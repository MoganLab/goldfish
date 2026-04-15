(import (liii check) (liii string))

;; string-every
;; 检查字符串中的每个字符是否都满足给定的条件。
;;
;; 语法
;; ----
;; (string-every char/pred? str)
;; (string-every char/pred? str start)
;; (string-every char/pred? str start end)
;;
;; 参数
;; ----
;; char/pred? : char 或 procedure?
;; - 字符(char)：检查字符串中的每个字符是否等于该字符
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
;; 如果字符串中的每个字符都满足条件则返回#t，否则返回#f。
;; 对于空字符串或空范围(如start=end)始终返回#t。
;; 对于多字节字符(如中文、emoji)，须确保谓词函数能正确处理UTF-8编码字符。
;; 当遇到第一个不满足条件的字符时，函数会立即返回#f，实现早期终止优化。
;;
;; 注意
;; ----
;; string-every支持多种类型的参数作为char/pred?，包括字符和谓词函数。
;; 当使用start/end参数时，检查对应子字符串的范围。
;; 空字符串或空范围会返回#t，因为没有任何字符违反条件。
;;
;; 示例
;; ----
;; (string-every #\x "xxxxxx") => #t
;; (string-every #\x "xxx0xx") => #f
;; (string-every char-numeric? "012345") => #t
;; (string-every char-numeric? "012d45") => #f
;; (string-every char-alphabetic? "abc") => #t
;; (string-every char-alphabetic? "abc123") => #f
;;
;; 错误处理
;; ----
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串时

;; 基本功能测试
(check-true (string-every #\x "xxxxxx"))
(check-false (string-every #\x "xxx0xx")
) ;check-false

(check-true (string-every char-numeric? "012345")
) ;check-true
(check-false (string-every char-numeric? "012d45")
) ;check-false

(check-true (string-every char-alphabetic? "abc")
) ;check-true
(check-false (string-every char-alphabetic? "abc123")
) ;check-false
(check-true (string-every char-upper-case? "ABC")
) ;check-true
(check-false (string-every char-upper-case? "AbC")
) ;check-false

(check-true (string-every char-whitespace? "   ")
) ;check-true
(check-false (string-every char-whitespace? "  a ")
) ;check-false

(check-true (string-every #\a ""))
(check-true (string-every char-numeric? "")
) ;check-true

(check-catch 'wrong-type-arg
  (string-every 1 "012345")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-every #\012345 "012345")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-every "012345" "012345")
) ;check-catch

(check-true (string-every char-numeric? "012345")
) ;check-true
(check-false (string-every number? "012345")
) ;check-false

(check-true (string-every char-numeric? "ab2345" 2)
) ;check-true
(check-false (string-every char-numeric? "ab2345" 1)
) ;check-false
(check-false (string-every char-numeric? "ab234f" 2)
) ;check-false
(check-true (string-every char-numeric?
              "ab234f"
              2
              4
            ) ;string-every
) ;check-true
(check-true (string-every char-numeric?
              "ab234f"
              2
              2
            ) ;string-every
) ;check-true
(check-false (string-every char-numeric?
               "ab234f"
               1
               4
             ) ;string-every
) ;check-false
(check-true (string-every char-numeric?
              "ab234f"
              2
              5
            ) ;string-every
) ;check-true
(check-false (string-every char-numeric?
               "ab234f"
               2
               6
             ) ;string-every
) ;check-false

(check-true (string-every #\a "aabbcc" 0 1)
) ;check-true
(check-false (string-every #\a "aabbcc" 1 3)
) ;check-false
(check-true (string-every char-lower-case?
              "abcABC"
              0
              3
            ) ;string-every
) ;check-true
(check-false (string-every char-lower-case?
               "abcABC"
               3
               6
             ) ;string-every
) ;check-false

(check-catch 'out-of-range
  (string-every char-numeric?
    "ab234f"
    2
    7
  ) ;string-every
) ;check-catch
(check-catch 'out-of-range
  (string-every char-numeric?
    "ab234f"
    2
    1
  ) ;string-every
) ;check-catch

;; 边界测试：空字符串必须返回#t
(check-true (string-every char-alphabetic? "")
) ;check-true
(check-true (string-every char-numeric? "")
) ;check-true
(check-true (string-every char-whitespace? "")
) ;check-true

;; 单字符边界测试
(check-true (string-every char-alphabetic? "a")
) ;check-true
(check-false (string-every char-numeric? "a")
) ;check-false
(check-true (string-every char-numeric? "9")
) ;check-true

;; 多字节字符测试（中文、emoji和UTF-8边界）
(check-true (string-every (lambda (c) #t)
              "一二三"
            ) ;string-every
) ;check-true
(check-true (string-every (lambda (c) #t)
              "😀😃😄😁"
            ) ;string-every
) ;check-true
(check-false (string-every char-alphabetic?
               "ab中文"
             ) ;string-every
) ;check-false

;; UTF-8边界测试: 空范围始终返回true
(check-true (string-every char-alphabetic?
              "abc"
              0
              0
            ) ;string-every
) ;check-true
(check-false (string-every char-alphabetic? "123abc")
) ;check-false

;; 特殊字符边界测试
(check-true (string-every char-whitespace?
              "\t\n\r "
            ) ;string-every
) ;check-true
(check-false (string-every char-numeric? "123\n45")
) ;check-false
(check-true (string-every (lambda (c) (not (char-whitespace? c)))
              "!@#$%^"
            ) ;string-every
) ;check-true

;; 全字符验证边界
(check-true (string-every (lambda (c) (char<=? #\A c #\Z))
              "ABCDEF"
            ) ;string-every
) ;check-true
(check-false (string-every char-lower-case? "ABCdef")
) ;check-false

;; 谓词为字符时边界测试
(check-true (string-every #\a ""))
(check-true (string-every #\a "a"))
(check-false (string-every #\a "ab"))

;; 大型字符串性能边界测试
(let ((big-string (make-string 5000 #\a)))
  (check-true (string-every char-alphabetic?
                big-string
              ) ;string-every
  ) ;check-true
) ;let

;; 早期终止验证测试（性能）
(let ((mixed-string (string-append (make-string 3000 #\a)
                      "b"
                      (make-string 2000 #\a)
                    ) ;string-append
      ) ;mixed-string
     ) ;
  (check-false (string-every #\a mixed-string)
  ) ;check-false
) ;let

;; 边界索引测试
(check-true (string-every char-numeric? "a1b2c" 1 2)
) ;check-true
(check-false (string-every char-numeric? "a1234" 0 5)
) ;check-false

(check-report)
