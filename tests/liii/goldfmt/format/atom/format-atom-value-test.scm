(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; format-atom-value
;; 将原子值格式化为显示文本：raw string 与字符字面量保留源码形式，
;; symbol 与 number 转字符串，其余用 write 语义。
;;
;; 语法
;; ----
;; (format-atom-value value)
;;
;; 参数
;; ----
;; value : any?
;;
;; 返回值
;; ------
;; string?

;; 测试：symbol 转为名字符串
(check (format-atom-value 'abc) => "abc")

;; 测试：number 转为数字字符串
(check (format-atom-value 42) => "42")

;; 测试：字符串用 write 语义带引号
(check (format-atom-value "abc") => "\"abc\"")

;; 测试：raw-string-literal 保留源码形式
(let ((lit (make-raw-string-literal :source "#\"x\"" :value "x")))
  (check (format-atom-value lit) => "#\"x\"")
) ;let

;; 测试：char-literal 保留源码形式
(let ((lit (make-char-literal :source "#\\a" :value #\a)))
  (check (format-atom-value lit) => "#\\a")
) ;let

;; 测试：boolean 用 write 语义
(check (format-atom-value #t) => "#t")

(check-report)
