;; emit-newline!
;; 输出换行符，行号加一、列号归零。
;;
;; 语法
;; ----
;; (emit-newline! writer)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; 返回值
;; ----
;; any?
;; 无返回值，该函数仅产生副作用。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：输出换行符
(let ((w (make-writer 0)))
  (emit-newline! w)
  (check (writer-result w) => "\n")
) ;let

;; 测试：行号加一、列号归零
(let ((w (make-writer 5)))
  (emit-newline! w)
  (check (writer-line w) => 2)
  (check (writer-column w) => 0)
) ;let

(check-report)
