;; make-writer
;; 创建带位置追踪的输出 writer，初始列为 initial-column。
;;
;; 语法
;; ----
;; (make-writer initial-column)
;;
;; 参数
;; ----
;; initial-column : integer?
;; writer 的初始列号，从 0 开始计数。
;;
;; 返回值
;; ----
;; vector?
;; 三元素向量：输出端口、行号、列号。
;;
;; 说明
;; ----
;; 1. 新建 writer 的行号为 1
;; 2. 文本输出由 `emit-string!`、`emit-newline!` 与 `emit-spaces!` 完成

(import (liii check) (liii goldfmt format writer))

(check-set-mode! 'report-failed)

;; 测试：初始行号为 1，列号为指定值
(let ((w (make-writer 5)))
  (check (writer-line w) => 1)
  (check (writer-column w) => 5)
  (check (writer-result w) => "")
) ;let

(check-report)
