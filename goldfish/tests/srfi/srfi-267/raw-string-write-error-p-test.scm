;; raw-string-write-error?
;; 判断对象是否为 raw string 写出错误。
;;
;; 语法
;; ----
;; (raw-string-write-error? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象，通常是 guard 捕获到的异常值。
;; 返回值
;; ----
;; boolean
;; obj 是 raw string 写出错误时返回 #t，否则返回 #f。
;; 说明
;; ----
;; 写出错误既可能是符号 raw-string-write-error，也可能是以其为 car 的列表。
;; 错误处理
;; ----
;; 无。

(import (liii check) (scheme file) (srfi srfi-267))


(check-set-mode! 'report-failed)


(check-false (raw-string-write-error? 'plain-symbol))
(check-false (raw-string-write-error? '()))
(check-false (raw-string-write-error? #f))
(check-false (raw-string-write-error? "raw-string-write-error"))

(check-true (raw-string-write-error? 'raw-string-write-error))
(check-true (raw-string-write-error? '(raw-string-write-error "boom")))

;; guard 捕获到的写出异常可以被识别
(let ((err (guard (e (#t e))
                 (write-raw-string "\"" "" (open-output-string))
                 'no-error
               ) ;guard
       ) ;err
      ) ;
  (check (raw-string-write-error? err) => #t)
) ;let

(check-report)
