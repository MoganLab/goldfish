;; emit-string!
;; 输出文本并更新 writer 的行号与列号。
;;
;; 语法
;; ----
;; (emit-string! writer text)
;;
;; 参数
;; ----
;; writer : vector?
;; 由 `make-writer` 创建的 writer。
;;
;; text : string?
;; 待输出的文本。
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

;; 测试：输出文本
(let ((w (make-writer 0)))
  (emit-string! w "abc")
  (check (writer-result w) => "abc")
  (check (writer-column w) => 3)
  (check (writer-line w) => 1)
) ;let

;; 测试：输出空文本不改变列号
(let ((w (make-writer 2)))
  (emit-string! w "")
  (check (writer-column w) => 2)
) ;let

;; 测试：输出含换行的文本后行号增加、列号重置
(let ((w (make-writer 0)))
  (emit-string! w "ab\ncd")
  (check (writer-line w) => 2)
  (check (writer-column w) => 2)
) ;let

(check-report)
