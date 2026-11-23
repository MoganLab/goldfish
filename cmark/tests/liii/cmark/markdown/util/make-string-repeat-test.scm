;; make-string-repeat
;; 生成 count 个字符 ch 组成的字符串。
;;
;; 语法
;; ----
;; (make-string-repeat ch count)
;;
;; 参数
;; ----
;; ch : char
;; 重复的字符。
;; count : integer
;; 非负重复次数。
;;
;; 返回值
;; ----
;; string
;; 由 count 个 ch 组成的字符串。
;;
;; 错误处理
;; ----
;; count 不是非负整数时抛出 type-error。

(import (scheme base))
(import (liii check))
(import (liii cmark markdown util))

(check-set-mode! 'report-failed)

(check-true (procedure? make-string-repeat))

(check (make-string-repeat #\` 1) => "`")
(check (make-string-repeat #\` 3) => "```")
(check (make-string-repeat #\- 0) => "")
(check-catch 'type-error (make-string-repeat #\` -1))
(check-catch 'type-error (make-string-repeat #\` 1.5))
(check-catch 'type-error (make-string-repeat #\` "3"))

(check-report)
