(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; make-string
;; 创建一个由指定字符重复填充的新字符串。
;;
;; 语法
;; ----
;; (make-string k [char])
;;
;; 参数
;; ----
;; k : exact?
;; 必须是非负的精确整数，表示要创建的字符串长度。
;;
;; char : char? 可选
;; 用于填充字符串的字符。如果未提供，**默认字符由实现定义**。
;;
;; 返回值
;; ------
;; string?
;; 新创建的字符串，长度为 k，所有字符均为 char（或实现定义的默认字符）。
;;
;; 说明
;; ----
;; 1. 可以指定字符串长度和填充字符
;; 2. 若未指定 char，**默认字符未在 R7RS 中定义**
;; 3. 当 k 为 0 时返回空字符串 ""
;;
;; 错误处理
;; --------
;; out-of-range
;; 当 k 为负数时抛出错误。
;; wrong-type-arg
;; 当 k 不是精确整数或 char 不是字符时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为 1 或 2 个时抛出错误。
(check (string-length (make-string 0))
  =>
  0
) ;check
(check (string-length (make-string 1))
  =>
  1
) ;check
(check (string-length (make-string 1000))
  =>
  1000
) ;check
(check (string-length (make-string 1000000))
  =>
  1000000
) ;check
(check (make-string 0 #\a) => "")
(check (make-string 1 #\a) => "a")
(check (string-length (make-string 1000 #\a))
  =>
  1000
) ;check
(let ((str (make-string 10000 #\a)))
  (check (string-length str) => 10000)
  (check (string-ref str 0) => #\a)
  (check (string-ref str 9999) => #\a)
) ;let
(check-catch 'out-of-range
  (make-string -1)
) ;check-catch
(check-catch 'out-of-range
  (make-string -5 #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-string 3.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (make-string 3 "a")
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-string)
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-string 3 #\a #\b)
) ;check-catch
(check (string->list "MathAgape")
  =>
  '(#\M #\a #\t #\h #\A #\g #\a #\p #\e)
) ;check
(check (string->list "") => '())
(check (list->string '(#\M #\a #\t #\h #\A #\g #\a #\p #\e)
       ) ;list->string
  =>
  "MathAgape"
) ;check
(check (list->string '()) => "")
(check (string-length "MathAgape") => 9)
(check (string-length "") => 0)
(check (catch 'wrong-type-arg
         (lambda ()
           (string-length 'not-a-string)
         ) ;lambda
         (lambda args #t)
       ) ;catch
  =>
  #t
) ;check
(check-report)