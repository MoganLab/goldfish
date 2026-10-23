(import (liii check))
(import (liii goldfmt format writer))

(check-set-mode! 'report-failed)

;; make-writer
;; 创建带位置追踪的输出 writer，初始列为 initial-column。
;;
;; 语法
;; ----
;; (make-writer initial-column)
;;
;; 返回值
;; ------
;; vector?
;; 三元素向量：输出端口、行号、列号。

;; 测试：初始行号为 1，列号为指定值
(let ((w (make-writer 5)))
  (check (writer-line w) => 1)
  (check (writer-column w) => 5)
) ;let

;; writer-result
;; 读取 writer 已输出的全部文本。
(let ((w (make-writer 0)))
  (emit-string! w "hello")
  (check (writer-result w) => "hello")
) ;let

;; emit-string!
;; 输出文本并更新行号列号；含换行时重置列号。
(let ((w (make-writer 0)))
  (emit-string! w "ab\ncd")
  (check (writer-line w) => 2)
  (check (writer-column w) => 2)
) ;let

;; emit-newline!
;; 输出换行符，行号加一、列号归零。
(let ((w (make-writer 3)))
  (emit-newline! w)
  (check (writer-line w) => 2)
  (check (writer-column w) => 0)
) ;let

;; emit-spaces!
;; 输出 n 个空格，列号相应增加；n 为 0 时无输出。
(let ((w (make-writer 0)))
  (emit-spaces! w 4)
  (check (writer-column w) => 4)
  (check (writer-result w) => "    ")
) ;let

(check-report)
