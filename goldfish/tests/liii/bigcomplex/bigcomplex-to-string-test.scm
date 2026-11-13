;; bigcomplex->string
;; 将 <bigcomplex> 转换为字符串表示。
;;
;; 语法
;; ----
;; (bigcomplex->string z [radix])
;;
;; 参数
;; ----
;; z : bigcomplex? 复数。
;; radix : integer? 可选进制，默认 10。
;;
;; 返回值
;; ----
;; string? 复数字符串。

(import (liii check) (liii bigcomplex) (liii bigint))

(check-set-mode! 'report-failed)

(check (bigcomplex->string (make-bigcomplex (string->bigint "100000000000000000001") 2))
  =>
  "100000000000000000001+2i"
) ;check
(check (bigcomplex->string (make-bigcomplex 0 -1)) => "-i")

(check-report)
