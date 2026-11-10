;; writer-line
;; 获取 writer 当前所在的行号。
;;
;; 语法
;; ----
;; (writer-line writer)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; 返回值
;; ----
;; integer?
;; 当前行号，从 1 开始计数。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：新建 writer 的行号为 1
(check (writer-line (make-writer 0)) => 1)

;; 测试：输出换行后行号加一
(let ((w (make-writer 0)))
  (emit-newline! w)
  (check (writer-line w) => 2)
) ;let

;; 测试：输出多行文本后行号按换行数增加
(let ((w (make-writer 0)))
  (emit-string! w "a\nb\nc")
  (check (writer-line w) => 3)
) ;let

(check-report)
