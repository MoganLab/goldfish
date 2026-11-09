;; string-range
;; 从字符串创建 range。
;;
;; 语法
;; ----
;; (string-range s)
;;
;; 参数
;; ----
;; s : string
;; 源字符串。
;;
;; 返回值
;; ----
;; range
;; 包含字符串字符的 range 对象。
;;
;; 注意
;; ----
;; 将字符串转换为字符向量后再创建 range。
;;
;; 错误处理
;; ----
;; - type-error：当 s 不是字符串时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (string-range "hello")))
  (check (range-length r) => 5)
  (check (range-ref r 0) => #\h)
  (check (range-ref r 4) => #\o)
) ;let

;; 空字符串合法
(check (range-length (string-range "")) => 0)

;; s 不是字符串时抛出 type-error
(check-catch 'type-error (string-range 42))
(check-catch 'type-error (string-range 'hello))
(check-catch 'type-error (string-range #(h e l l o)))
(check-catch 'type-error (string-range #\a))


(check-report)
