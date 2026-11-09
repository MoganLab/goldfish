;; raw-string-read-error?
;; 判断对象是否为 raw string 读取错误。
;;
;; 语法
;; ----
;; (raw-string-read-error? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象，通常是 guard 捕获到的异常值。
;; 返回值
;; ----
;; boolean
;; obj 是 raw string 读取错误时返回 #t，否则返回 #f。
;; 说明
;; ----
;; 读取错误既可能是符号 raw-string-read-error，也可能是以其为 car 的列表。
;; 错误处理
;; ----
;; 无。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(check-false (raw-string-read-error? 'plain-symbol))
(check-false (raw-string-read-error? '()))
(check-false (raw-string-read-error? #f))
(check-false (raw-string-read-error? "raw-string-read-error"))

(check-true (raw-string-read-error? 'raw-string-read-error))
(check-true (raw-string-read-error? '(raw-string-read-error "boom")))

;; guard 捕获到的读取异常可以被识别
(let ((err (guard (e (#t e)) (read-raw-string (open-input-string "#x")) 'no-error)))
  (check (raw-string-read-error? err) => #t)
) ;let

(check-report)
