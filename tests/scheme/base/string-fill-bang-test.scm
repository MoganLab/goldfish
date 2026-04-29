(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-fill!
;; 将字符串的所有字符替换为指定字符。
;;
;; 语法
;; ----
;; (string-fill! string char)
;;
;; 参数
;; ----
;; string : string?
;; 待修改的字符串。
;; char : char?
;; 用于填充的字符。
;;
;; 返回值
;; ------
;; 未指定（unspecified）。
;;
;; 说明
;; ----
;; 1. 修改原字符串的所有位置
;; 2. 空字符串调用无效果
;; 3. 返回值为未指定
(let ((s (make-string 3 #\a)))
  (string-fill! s #\b)
  (check s => "bbb")
) ;let
(let ((s (make-string 0)))
  (string-fill! s #\x)
  (check s => "")
) ;let
(let ((s (make-string 5 #\0)))
  (string-fill! s #\1)
  (check s => "11111")
) ;let
(let ((s (make-string 5 #\a)))
  (string-fill! s #\x 1 4)
  (check s => "axxxa")
) ;let
(check-catch 'wrong-type-arg (string-fill! '() #\a))
(check-catch 'wrong-type-arg (string-fill! "abc" 'a))
(check-catch 'wrong-number-of-args (string-fill! "abc"))
(check-catch 'wrong-type-arg (string-fill! "abc" #\a #\b))

(check-report)
