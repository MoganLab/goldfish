(import (liii check) (liii string))

;; string-null?
;; 判断一个字符串是否为空字符串。
;;
;; 语法
;; ----
;; (string-null? str)
;;
;; 参数
;; ----
;; str : string?
;; 要检查的字符串。可以是s7字符串或其它自动转换为字符串的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果str是空字符串("")则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; string-null?主要用于测试字符串是否为零长度。字符串为空字符串的标准是
;; 其长度为0。字符串非字符串类型的参数会引发错误。
;;
;; 示例
;; ----
;; (string-null? "") => #t
;; (string-null? "a") => #f
;; (string-null? " ") => #f
;;
;; 错误处理
;; ----
;; type-error 当str不是字符串类型时

;; 基本功能测试
(check-true (string-null? ""))
(check-true (string-null? (make-string 0))
) ;check-true
(check-true (string-null? (string)))
(check-true (string-null? (string-copy ""))
) ;check-true

(check-false (string-null? "a"))
(check-false (string-null? " "))
(check-false (string-null? (string #\null))
) ;check-false
(check-false (string-null? "aa"))
(check-false (string-null? "中文"))
(check-false (string-null? "123"))
(check-false (string-null? "MathAgape"))

;; 中文字符单字符非空验证
(check-false (string-null? "中"))
(check-false (string-null? "文"))

;; emoji字符非空验证
(check-false (string-null? "☀"))
(check-false (string-null? "❤"))

;; 特殊转义字符非空验证
(check-false (string-null? "\t"))
(check-false (string-null? "\n"))
(check-false (string-null? "\x00;"))

;; 非字符串类型错误测试
(check-catch 'type-error
  (string-null? 'not-a-string)
) ;check-catch
(check-catch 'type-error
  (string-null? 123)
) ;check-catch
(check-catch 'type-error
  (string-null? #\a)
) ;check-catch
(check-catch 'type-error
  (string-null? (list "a"))
) ;check-catch
(check-catch 'type-error
  (string-null? #f)
) ;check-catch
(check-catch 'type-error
  (string-null? '())
) ;check-catch
(check-catch 'type-error
  (string-null? (vector))
) ;check-catch
(check-catch 'type-error
  (string-null? (make-vector 0))
) ;check-catch
(check-catch 'type-error
  (string-null? 0)
) ;check-catch
(check-catch 'type-error
  (string-null? 42)
) ;check-catch
(check-catch 'type-error
  (string-null? 3.14)
) ;check-catch

;; 性能边界测试：大字符串非空验证
(let ((large-str (make-string 1000000 #\A)))
  (check-false (string-null? large-str))
) ;let

(check-report)
