;; writer-column
;; 获取 writer 当前所在的列号。
;;
;; 语法
;; ----
;; (writer-column writer)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; 返回值
;; ----
;; integer?
;; 当前列号，从 0 开始计数。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

;; 测试：新建 writer 的列号为指定值
(check (writer-column (make-writer 5)) => 5)

;; 测试：输出文本后列号相应增加
(let ((w (make-writer 0)))
  (emit-string! w "abc")
  (check (writer-column w) => 3)
) ;let

;; 测试：输出换行后列号归零
(let ((w (make-writer 3)))
  (emit-newline! w)
  (check (writer-column w) => 0)
) ;let

(check-report)
