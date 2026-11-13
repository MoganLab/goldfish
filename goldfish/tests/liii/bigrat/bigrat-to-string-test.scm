;; bigrat->string
;; 将 <bigrat> 转换为字符串表示。
;;
;; 语法
;; ----
;; (bigrat->string r [radix])
;;
;; 参数
;; ----
;; r : bigrat? 有理数。
;; radix : integer? 可选进制，默认 10。
;;
;; 返回值
;; ----
;; string? 分数字符串。

(import (liii check) (liii bigrat) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigrat->string (make-bigrat (string->bigint "100000000000000000000") 3))
  =>
  "100000000000000000000/3"
) ;check
(check (bigrat->string (make-bigrat 255 16) 16) => "ff/10")

(check-report)
