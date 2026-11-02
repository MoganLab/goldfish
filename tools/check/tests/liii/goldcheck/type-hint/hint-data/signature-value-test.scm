;; signature-value
;; 从变量定义记录中提取其对应的签名数据（脱去 quote 外壳）。
;;
;; 语法
;; ----
;; (signature-value d)
;;
;; 参数
;; ----
;; d : def-record   变量定义记录
;;
;; 返回值
;; ------
;; any
;; 提取出的签名列表数据。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 提取带 quote 的签名数据
(let* ((d '(hint-foo value #f 1 (define hint-foo '((any? -> any?))))))
  (check (signature-value d) => '((any? -> any?)))
) ;let*

(check-report)
