;; parse-math-block
;; 把块级数学公式行序列解析为 math_block 节点。
;;
;; 语法
;; ----
;; (parse-math-block lines)
;;
;; 参数
;; ----
;; lines : list
;; 由起始标记、内容行与结束标记组成的字符串列表。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回类型为 'math_block 的节点，literal 字段保存公式内容。

(import (liii check) (liii cmark inline math) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((math (parse-math-block '("$$" "E = mc^2" "$$"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "E = mc^2")
) ;let

(let ((math (parse-math-block '("$$" "a^2 + b^2 = c^2" "x = y" "$$"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math)))
    =>
    "a^2 + b^2 = c^2\nx = y"
  ) ;check
) ;let

(let ((math (parse-math-block '("```math" "\\int_0^1 x^2\\,dx" "```"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "\\int_0^1 x^2\\,dx")
) ;let

(let ((math (parse-math-block '("```math" "A_{ij}"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "A_{ij}")
) ;let

(let ((math (parse-math-block '())))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "")
) ;let

;; \[...\] 单行块
(let ((math (parse-math-block '("\\[E = mc^2\\]"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "E = mc^2")
) ;let

;; \[ 多行块（collector 已含 \\] 结束行）
(let ((math (parse-math-block '("\\[" "a^2 + b^2 = c^2" "x = y" "\\]"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math)))
    =>
    "a^2 + b^2 = c^2\nx = y"
  ) ;check
) ;let

;; \[ 未闭合：消费到末尾（与 $$ 未闭合行为一致）
(let ((math (parse-math-block '("\\[" "A_{ij}"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "A_{ij}")
) ;let

;; $$...$$ 单行块（0113：独占一行的单行展示公式）
(let ((math (parse-math-block '("$$E = mc^2$$"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "E = mc^2")
) ;let

(let ((math (parse-math-block '("$$ \\int_0^1 x^2\\,dx $$"))))
  (check (cmark-node-type math) => 'math_block)
  (check (cdr (assoc 'literal (cmark-node-data math))) => "\\int_0^1 x^2\\,dx")
) ;let

(check-report)
