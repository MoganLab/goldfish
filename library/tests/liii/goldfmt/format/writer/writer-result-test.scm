;; writer-result
;; 读取 writer 已输出的全部文本。
;;
;; 语法
;; ----
;; (writer-result writer)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; 返回值
;; ----
;; string?
;; 已输出的文本内容。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：新建 writer 的结果为空字符串
(check (writer-result (make-writer 0)) => "")

;; 测试：输出文本后可以读回
(let ((w (make-writer 0)))
  (emit-string! w "hello")
  (check (writer-result w) => "hello")
) ;let

;; 测试：多次输出的文本按顺序拼接
(let ((w (make-writer 0)))
  (emit-string! w "a")
  (emit-newline! w)
  (emit-string! w "b")
  (check (writer-result w) => "a\nb")
) ;let

(check-report)
