;; display-stderr
;; 向标准错误输出一行文本。
;;
;; 语法
;; ----
;; (display-stderr msg)
;;
;; 参数
;; ----
;; msg : string
;; 待输出的文本。
;;
;; 返回值
;; ----
;; 不返回
;; 向标准错误端口输出文本并换行。

(import (liii check)
  (liii pi code modes output)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证为过程对象
(check-true (procedure? display-stderr))

;; 2. 正常调用不抛错
(display-stderr "warning message")
(check-true #t)

(check-report)
