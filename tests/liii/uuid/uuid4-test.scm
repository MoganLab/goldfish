(import (liii check)
        (liii uuid)
) ;import

(check-set-mode! 'report-failed)

;; uuid4
;; 生成一个随机的 UUID v4 字符串。
;;
;; 语法
;; ----
;; (uuid4)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string
;; 返回一个 36 字符的 UUID v4 字符串，格式为 xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx。
;;
;; 注意
;; ----
;; 每次调用都会生成一个新的随机 UUID。
;;
;; 示例
;; ----
;; (uuid4) => "550e8400-e29b-41d4-a716-446655440000"
;;
;; 错误处理
;; ------
;; 无异常抛出

(check (string-length (uuid4)) => 36)

;; Verify format: 8-4-4-4-12 pattern
(define uuid-str (uuid4))
(check (char=? (string-ref uuid-str 8) #\-) => #t)
(check (char=? (string-ref uuid-str 13) #\-) => #t)
(check (char=? (string-ref uuid-str 18) #\-) => #t)
(check (char=? (string-ref uuid-str 23) #\-) => #t)

;; Verify version bit (4th character of 3rd segment should be '4')
(check (char=? (string-ref uuid-str 14) #\4) => #t)

(check-report)
