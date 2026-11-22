;; make-cmark-code-block
;; 创建块级代码节点。
;;
;; 语法
;; ----
;; (make-cmark-code-block info literal)
;;
;; 参数
;; ----
;; info : any
;; 输入参数。
;; literal : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node
;; 返回新创建的 cmark AST 节点。

(import (liii check) (liii cmark node))

(check-set-mode! 'report-failed)

(let ((node (make-cmark-code-block "scheme" "(define x 1)")))
  (check (cmark-node-type node) => 'code_block)
  (check (assoc 'info (cmark-node-data node)) => '(info . "scheme"))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . "(define x 1)"))
  (check (cmark-node-children node) => '())
  (check (cmark-node-parent node) => #f)
) ;let

;; 测试空 info（缩进代码块或无名围栏代码块）
(let ((node (make-cmark-code-block "" "line 1\nline 2")))
  (check (assoc 'info (cmark-node-data node)) => '(info . ""))
  (check (assoc 'literal (cmark-node-data node)) => '(literal
                                                       . "line 1\nline 2"))
) ;let

;; 测试空 literal
(let ((node (make-cmark-code-block "python" "")))
  (check (assoc 'literal (cmark-node-data node)) => '(literal . ""))
) ;let

;; 测试多行代码内容
(let ((node (make-cmark-code-block "bash" "echo hello\necho world\necho done")))
  (check (assoc 'literal (cmark-node-data node))
    =>
    '(literal . "echo hello\necho world\necho done")
  ) ;check
) ;let

;; 测试 info string 含空格（如 ```python script）
(let ((node (make-cmark-code-block "python script" "print(1)")))
  (check (assoc 'info (cmark-node-data node)) => '(info . "python script"))
) ;let

(check-report)
